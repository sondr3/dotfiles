export interface Context {
  verbose: boolean;
  info: boolean;
}

declare global {
  interface Window {
    context: Context;
  }
}

export function buildContext(verbose: boolean): Context {
  return {
    verbose: verbose,
    info: false,
  };
}
