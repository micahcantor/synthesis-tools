export type Result<T, E = Error> =
  | { ok: true; value: T }
  | { ok: false; error: E };

export function Ok<T>(value: T): Result<T> {
  return {ok: true, value: value};
}

export function Err<T, E extends Error>(error: E): Result<T> {
  return {ok: false, error: error}
}
