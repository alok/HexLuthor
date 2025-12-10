import * as vscode from 'vscode';

// Regex to find CANDIDATE positions - we validate them via LSP
const HEX_COLOR_REGEX = /#x([0-9A-Fa-f]{6})\b/g;

// Cache decoration types by color
const decorationCache = new Map<string, vscode.TextEditorDecorationType>();

function getDecorationForColor(hexColor: string): vscode.TextEditorDecorationType {
  const cached = decorationCache.get(hexColor);
  if (cached) return cached;

  const decoration = vscode.window.createTextEditorDecorationType({
    color: `#${hexColor}`,
    fontWeight: 'bold',
  });

  decorationCache.set(hexColor, decoration);
  return decoration;
}

// Check if position is a Hex type via Lean LSP hover
async function isHexType(document: vscode.TextDocument, position: vscode.Position): Promise<boolean> {
  try {
    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      'vscode.executeHoverProvider',
      document.uri,
      position
    );

    if (!hovers || hovers.length === 0) return false;

    // Check if any hover content mentions "Hex" type
    for (const hover of hovers) {
      for (const content of hover.contents) {
        const text = typeof content === 'string'
          ? content
          : (content as vscode.MarkdownString).value;

        // Lean's hover shows type info like ": Hex" or "Hex"
        if (text && (text.includes(': Hex') || text.match(/\bHex\b/))) {
          return true;
        }
      }
    }
  } catch {
    // LSP not ready or error - skip
  }
  return false;
}

async function updateDecorations(editor: vscode.TextEditor) {
  if (!editor.document.fileName.endsWith('.lean')) return;

  const text = editor.document.getText();
  const colorRanges = new Map<string, vscode.Range[]>();

  // Find all candidate positions
  const candidates: { hexValue: string; range: vscode.Range; position: vscode.Position }[] = [];

  let match: RegExpExecArray | null;
  const regex = new RegExp(HEX_COLOR_REGEX.source, 'g');

  while ((match = regex.exec(text)) !== null) {
    const hexValue = match[1].toUpperCase();
    const startPos = editor.document.positionAt(match.index);
    const endPos = editor.document.positionAt(match.index + match[0].length);
    candidates.push({
      hexValue,
      range: new vscode.Range(startPos, endPos),
      position: startPos
    });
  }

  // Validate candidates via LSP (batched to avoid overload)
  const BATCH_SIZE = 10;
  const validations: typeof candidates & { isValid: boolean }[] = [];

  for (let i = 0; i < candidates.length; i += BATCH_SIZE) {
    const batch = candidates.slice(i, i + BATCH_SIZE);
    const results = await Promise.all(
      batch.map(async c => ({
        ...c,
        isValid: await isHexType(editor.document, c.position)
      }))
    );
    validations.push(...results);
  }

  // Group valid ranges by color
  for (const { hexValue, range, isValid } of validations) {
    if (isValid) {
      if (!colorRanges.has(hexValue)) {
        colorRanges.set(hexValue, []);
      }
      colorRanges.get(hexValue)!.push(range);
    }
  }

  // Clear old decorations
  for (const [color, decoration] of decorationCache) {
    if (!colorRanges.has(color)) {
      editor.setDecorations(decoration, []);
    }
  }

  // Apply decorations
  for (const [hexColor, ranges] of colorRanges) {
    const decoration = getDecorationForColor(hexColor);
    editor.setDecorations(decoration, ranges);
  }
}

// Debounce to avoid spamming LSP
let updateTimeout: NodeJS.Timeout | undefined;
function scheduleUpdate(editor: vscode.TextEditor) {
  if (updateTimeout) clearTimeout(updateTimeout);
  updateTimeout = setTimeout(() => updateDecorations(editor), 300);
}

// Color provider for hover palette (allows picking colors)
// Note: The small swatches are required for the picker to work
class HexLuthorColorProvider implements vscode.DocumentColorProvider {
  provideDocumentColors(
    document: vscode.TextDocument,
    _token: vscode.CancellationToken
  ): vscode.ProviderResult<vscode.ColorInformation[]> {
    if (!document.fileName.endsWith('.lean')) return [];

    const colors: vscode.ColorInformation[] = [];
    const text = document.getText();
    const regex = new RegExp(HEX_COLOR_REGEX.source, 'g');

    let match: RegExpExecArray | null;
    while ((match = regex.exec(text)) !== null) {
      const hexValue = match[1];
      const startPos = document.positionAt(match.index);
      const endPos = document.positionAt(match.index + match[0].length);
      const range = new vscode.Range(startPos, endPos);

      const r = parseInt(hexValue.substring(0, 2), 16) / 255;
      const g = parseInt(hexValue.substring(2, 4), 16) / 255;
      const b = parseInt(hexValue.substring(4, 6), 16) / 255;

      colors.push(new vscode.ColorInformation(range, new vscode.Color(r, g, b, 1)));
    }

    return colors;
  }

  provideColorPresentations(
    color: vscode.Color,
    context: { document: vscode.TextDocument; range: vscode.Range },
    _token: vscode.CancellationToken
  ): vscode.ProviderResult<vscode.ColorPresentation[]> {
    const r = Math.round(color.red * 255);
    const g = Math.round(color.green * 255);
    const b = Math.round(color.blue * 255);

    const hexString = [r, g, b]
      .map(c => c.toString(16).padStart(2, '0').toUpperCase())
      .join('');

    const presentation = new vscode.ColorPresentation(`#x${hexString}`);
    presentation.textEdit = new vscode.TextEdit(context.range, `#x${hexString}`);

    return [presentation];
  }
}

export function activate(context: vscode.ExtensionContext) {
  console.log('HexLuthor Color Preview activated (LSP-driven)');

  // Register color provider for hover palette
  const colorProvider = new HexLuthorColorProvider();
  context.subscriptions.push(
    vscode.languages.registerColorProvider({ language: 'lean4', scheme: 'file' }, colorProvider),
    vscode.languages.registerColorProvider({ language: 'lean', scheme: 'file' }, colorProvider)
  );

  const updateActiveEditor = () => {
    const editor = vscode.window.activeTextEditor;
    if (editor) scheduleUpdate(editor);
  };

  updateActiveEditor();

  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor(updateActiveEditor),
    vscode.workspace.onDidChangeTextDocument(e => {
      const editor = vscode.window.activeTextEditor;
      if (editor && e.document === editor.document) {
        scheduleUpdate(editor);
      }
    })
  );
}

export function deactivate() {
  if (updateTimeout) clearTimeout(updateTimeout);
  for (const decoration of decorationCache.values()) {
    decoration.dispose();
  }
  decorationCache.clear();
}
