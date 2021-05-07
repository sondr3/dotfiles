declare global {
  interface Window {
    context: Context;
  }
}

export class Context {
  private _verbose: boolean;
  private _info: boolean;

  constructor(verbose: boolean) {
    this._verbose = verbose;
    this._info = false;
  }

  public get verbose(): boolean {
    return this._verbose;
  }

  public set verbose(v: boolean) {
    this.verbose = v;
  }

  public get info(): boolean {
    return this._info;
  }

  public set info(v: boolean) {
    this._info = v;
  }
}
