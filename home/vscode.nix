{ pkgs, ... }:

{
  programs.vscode = {
    enable = true;
    userSettings = {
      "editor.fontSize" = 16;
      "editor.fontLigatures" = true;
      "editor.fontFamily" = "PragmataPro Mono Liga";
      "editor.tabSize" = 2;
      "editor.lineNumbers" = "relative";
      "editor.formatOnSave" = true;
      "editor.formatOnPaste" = true;
      "editor.defaultFormatter" = "esbenp.prettier-vscode";
      "editor.suggestSelection" = "first";
      "workbench.settings.useSplitJSON" = true;
      "workbench.settings.editor" = "json";
      "vim.leader" = "<space>";
      "vim.easymotion" = true;
      "vim.sneak" = true;
      "vim.incsearch" = true;
      "vim.useSystemClipboard" = true;
      "vim.useCtrlKeys" = true;
      "vim.hlsearch" = true;
      "vim.insertModeKeyBindings" = [
        {
          "before" = ["j" "k"];
          "after" = ["<Esc>"];
        }
      ];
      "eslint.autoFixOnSave" = true;
      "eslint.validate" = [
        "javascript"
        "javascriptreact"
        {
          "language" = "typescript";
          "autoFix" = true;
        }
        {
          "language" = "typescriptreact";
          "autoFix" = true;
        }
      ];
      "prettier.eslintIntegration" = true;
      "prettier.stylelintIntegration" = true;
      "explorer.confirmDelete" = false;
      "workbench.statusBar.feedback.visible" = false;
      "workbench.colorTheme" = "Default Light+";
      "workbench.iconTheme" = "vscode-icons";
      "search.showLineNumbers" = true;
      "javascript.updateImportsOnFileMove.enabled" = "always";
      "typescript.updateImportsOnFileMove.enabled" = "always";
      "update.mode" = "none";
      "telemetry.enableTelemetry" = false;
    };
    extensions = [
      # cmstead.jsrefactor
      # CoenraadS.bracket-pair-colorizer
      # Compulim.compulim-vscode-closetag
      # dbaeumer.vscode-eslint
      # esbenp.prettier-vscode
      # formulahendry.auto-rename-tag
      # jpoissonnier.vscode-styled-components
      # mgmcdermott.vscode-language-babel
      # naumovs.color-highlight
      # shinnn.stylelint
      # silvenon.mdx
      # vscode-icons-team.vscode-icons
      # vscodevim.vim
    ];
  };
}
