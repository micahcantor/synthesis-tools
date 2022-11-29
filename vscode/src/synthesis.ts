import * as cp from "child_process";
import * as util from "util";
import { Ok, Err, Result } from "./result";

const exec = util.promisify(cp.exec);

/* Call into cabal to execute the synthesis program */
export async function runSynthesis(fileName: string, input: string): Promise<Result<string>> {
  // TODO: sanitize input?
  const { stdout, stderr } = await exec(`ghc-hacking ${fileName} ${input}`);
  if (stderr != "")
    return Err(new Error(stdout));
  else
    return Ok(stdout);
}