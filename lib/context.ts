export interface Context {
  verbose: boolean;
}

declare global {
  interface Window {
    context: Context;
  }
}

export function defaultContext(): Context {
  return {
    verbose: false,
  };
}
