-- Package manager, use PaqInstall to install
require "paq" {
	"savq/paq-nvim";                  -- Let Paq manage itself
	'neomake/neomake'; --linting
	'janko-m/vim-test'; -- test for ruby
	'peitalin/vim-jsx-typescript'; --typescript
	{'nvim-treesitter/nvim-treesitter', build=':TSUpdate' };
	'nvim-treesitter/playground';
	'neovim/nvim-lspconfig'; -- language server
 	'junegunn/fzf'; -- built-in fzf plugin
	'junegunn/fzf.vim'; -- more advanced plugin built on top of built-in one
	'simrat39/rust-tools.nvim'; -- advanced feature from rust-analyzser using the LSP
	'mileszs/ack.vim';
	'github/copilot.vim'; -- copilot setup
}

vim.api.nvim_exec("call neomake#configure#automake('nrwi', 500)", false)

local map = vim.api.nvim_set_keymap

-- map method takes 4 arguments: the mode for which the mapping will take effect, the key sequence, the command to execute and a table of options
map('n', ',', '', {})
vim.g.mapleader = ','  -- 'vim.g' sets global variables

local o = vim.opt
local wo = vim.wo
local bo = vim.bo

o.mouse = "" -- neovim defaults to "nvi" (so activating mouse support everywhere)
-- but it conflicts with the idea of using the mouse just to select code

o.ignorecase = true -- when searching, ignore case
o.number = true -- display line numbers

o.tabstop = 2 -- number of visual spaces per tab
o.softtabstop = 2 -- number of spaces in tab when editing
o.shiftwidth = 2 -- number of spaces to use for autoindent
o.expandtab = true -- tabs are spaces
o.autoindent = true
o.copyindent = true -- copy indent from previous line

local cmd = vim.cmd
cmd("command W w !sudo tee % > /dev/null")

options = { noremap = true }
map('n', '<leader>t', ':TestNearest<cr>', options)
map('n', '<leader>T', ':TestFile<cr>', options)

-- open the full error view
map('n', '<leader>e', ':lua vim.diagnostic.open_float({})<cr>', options)

-- fzf special config
map('n', ';', ':Buffers<cr>', options)
map('n', '<leader>;', ':Files<cr>', options)
-- end of fzf config

require'nvim-treesitter.configs'.setup {
  ensure_installed = {"ruby", "python"}, -- one of "all" or a list of languages
  ignore_install = {}, -- List of parsers to ignore installing
  highlight = {
    enable = true,              -- false will disable the whole extension
    disable = {},  -- list of language that will be disabled
    additional_vim_regex_highlighting = true, -- https://github.com/nvim-treesitter/nvim-treesitter/issues/1501
  },
  indent = {
    -- enable = true
  }
}

require'lspconfig'.solargraph.setup{}
require'lspconfig'.pyright.setup{}

local nvim_lsp = require('lspconfig')

-- Use an on_attach function to only map the following keys 
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
  local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

  --Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  local opts = { noremap=true, silent=true }

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
  buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)

  vim.keymap.set("n", "g[", vim.diagnostic.goto_prev, opts)
  vim.keymap.set("n", "g]", vim.diagnostic.goto_next, opts)
end



-- Use a loop to conveniently call 'setup' on multiple servers and
-- map buffer local keybindings when the language server attaches
local servers = { "pyright", "rust_analyzer", "solargraph" }
for _, lsp in ipairs(servers) do
  nvim_lsp[lsp].setup {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  }
end


-- firenvim settings
vim.g.firenvim_config = {localSettings = { ['.*'] = {} } , globalSettings = {}}
vim.g.firenvim_config['globalSettings'] = { takeover = 'never', priority = 1 }

local rt = require("rust-tools")

rt.setup({
  server = {
    on_attach = function(_, bufnr)
      -- Hover actions
      vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
      -- Code action groups
      vim.keymap.set("n", "<Leader>a", rt.code_action_group.code_action_group, { buffer = bufnr })
    end,
  },
})


vim.g.ackprg = 'ag --vimgrep'

vim.g.copilot_filetypes = {
  ['*'] = true, -- enable copilot for all filetypes
  ['dotenv'] = true, -- avoid sending passwords
}
