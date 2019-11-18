{ config, lib, ... }:

with lib;

{
  options.variables = mkOption {
    type = types.attrs;
    default = {};
  };

  config._module.args.variables = config.variables;

}
