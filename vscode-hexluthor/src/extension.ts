import * as vscode from 'vscode';

// Regex to match #xRRGGBB syntax in Lean files (no quotes!)
const HEX_COLOR_REGEX = /#x([0-9A-Fa-f]{6})\b/g;

// Cache decoration types to avoid recreating them
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

function updateDecorations(editor: vscode.TextEditor) {
  if (!editor.document.fileName.endsWith('.lean')) return;

  const text = editor.document.getText();

  // Group ranges by color
  const colorRanges = new Map<string, vscode.Range[]>();

  let match: RegExpExecArray | null;
  const regex = new RegExp(HEX_COLOR_REGEX.source, 'g');

  while ((match = regex.exec(text)) !== null) {
    const hexValue = match[1].toUpperCase();
    const startPos = editor.document.positionAt(match.index);
    const endPos = editor.document.positionAt(match.index + match[0].length);
    const range = new vscode.Range(startPos, endPos);

    if (!colorRanges.has(hexValue)) {
      colorRanges.set(hexValue, []);
    }
    colorRanges.get(hexValue)!.push(range);
  }

  // Clear old decorations not in current document
  for (const [color, decoration] of decorationCache) {
    if (!colorRanges.has(color)) {
      editor.setDecorations(decoration, []);
    }
  }

  // Apply decorations grouped by color
  for (const [hexColor, ranges] of colorRanges) {
    const decoration = getDecorationForColor(hexColor);
    editor.setDecorations(decoration, ranges);
  }
}

// Also keep DocumentColorProvider for the color picker UI
class HexLuthorColorProvider implements vscode.DocumentColorProvider {
  provideDocumentColors(
    document: vscode.TextDocument,
    _token: vscode.CancellationToken
  ): vscode.ProviderResult<vscode.ColorInformation[]> {
    const colors: vscode.ColorInformation[] = [];
    const text = document.getText();

    let match: RegExpExecArray | null;
    const regex = new RegExp(HEX_COLOR_REGEX.source, 'g');

    while ((match = regex.exec(text)) !== null) {
      const hexValue = match[1];
      const startPos = document.positionAt(match.index);
      const endPos = document.positionAt(match.index + match[0].length);
      const range = new vscode.Range(startPos, endPos);

      const r = parseInt(hexValue.substring(0, 2), 16) / 255;
      const g = parseInt(hexValue.substring(2, 4), 16) / 255;
      const b = parseInt(hexValue.substring(4, 6), 16) / 255;

      const color = new vscode.Color(r, g, b, 1);
      colors.push(new vscode.ColorInformation(range, color));
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
  console.log('HexLuthor Color Preview activated');

  // Register color provider for color picker (2-way editing)
  const colorProvider = new HexLuthorColorProvider();
  context.subscriptions.push(
    vscode.languages.registerColorProvider({ language: 'lean4', scheme: 'file' }, colorProvider),
    vscode.languages.registerColorProvider({ language: 'lean', scheme: 'file' }, colorProvider)
  );

  // Update decorations on editor changes
  const updateActiveEditor = () => {
    const editor = vscode.window.activeTextEditor;
    if (editor) updateDecorations(editor);
  };

  // Initial update
  updateActiveEditor();

  // Subscribe to events
  context.subscriptions.push(
    vscode.window.onDidChangeActiveTextEditor(updateActiveEditor),
    vscode.workspace.onDidChangeTextDocument(e => {
      const editor = vscode.window.activeTextEditor;
      if (editor && e.document === editor.document) {
        updateDecorations(editor);
      }
    })
  );
}

export function deactivate() {
  // Clean up decoration types
  for (const decoration of decorationCache.values()) {
    decoration.dispose();
  }
  decorationCache.clear();
}
