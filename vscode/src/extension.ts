// The module 'vscode' contains the VS Code extensibility API
// Import the module and reference it with the alias vscode in your code below
import * as vscode from 'vscode';
import { runSynthesis } from './synthesis';
import { Ok, Err, Result } from "./result";

// This method is called when your extension is activated
// Extension is activated when a Haskell file is opened
export function activate(context: vscode.ExtensionContext) {
	console.log('activating');
	context.subscriptions.push(
		vscode.languages.registerCodeActionsProvider('haskell', new Synthesizer(), {
			providedCodeActionKinds: Synthesizer.providedCodeActionKinds
		}));
}

export class Synthesizer implements vscode.CodeActionProvider {
	public static readonly providedCodeActionKinds = [
		vscode.CodeActionKind.QuickFix
	];

	public async provideCodeActions(document: vscode.TextDocument, range: vscode.Range): Promise<vscode.CodeAction[] | undefined> {
		const input = document.getText(range);
		if (input.startsWith("run")) { // extremely hacky
			const synthesizeDefinitionResult = await this.createFix(document, range, input);
			if (synthesizeDefinitionResult.ok) {
				return [synthesizeDefinitionResult.value];
			}
			else {
				console.error(synthesizeDefinitionResult.error);
			}
		}
	}

	private async createFix(document: vscode.TextDocument, range: vscode.Range, input: string): Promise<Result<vscode.CodeAction>> {
		const fix = new vscode.CodeAction('Generate definition...', vscode.CodeActionKind.QuickFix);
		const synthesisResult = await runSynthesis(document.fileName, input);
		if (synthesisResult.ok) {
			const value = synthesisResult.value;
			fix.edit = new vscode.WorkspaceEdit();
			const replaceRange = new vscode.Range(range.end.translate(0, 3), range.end.translate(0, 3 + value.length))
			fix.edit.replace(document.uri, replaceRange, value);
			return Ok(fix);
		} else {
			return Err(synthesisResult.error);
		}

	}
}

// This method is called when your extension is deactivated
export function deactivate() { }
