self: super:

{
  awesome = super.awesome.override {
    luaPackages = super.luajitPackages;
    gtk3Support = true;
    gtk3 = self.pkgs.gtk3;
  };
}
