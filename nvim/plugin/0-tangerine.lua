-- pick your plugin manager, default [standalone]
local pack = "packer"

local function bootstrap (url)
	local name = url:gsub(".*/", "")
	local path = vim.fn.stdpath [[data]] .. "/site/pack/".. pack .. "/start/" .. name

	if vim.fn.isdirectory(path) == 0 then
		print(name .. ": installing in data dir...")

		vim.fn.system {"git", "clone", "--depth", "1", url, path}

		vim.cmd [[redraw]]
		print(name .. ": finished installing")
	end
end

bootstrap "https://github.com/wbthomason/packer.nvim"
bootstrap "https://github.com/udayvir-singh/tangerine.nvim"


local nvim_dir = vim.fn.stdpath [[config]]

require'tangerine'.setup {
	vimrc   = nvim_dir .. "/fnl/init.fnl",
	source  = nvim_dir .. "/fnl",
	-- target  = nvim_dir .. "/lua",
	target = vim.fn.stdpath [[data]] .. "/tangerine",
	-- target = vim.fn.stdpath [[data]] .. "/tangerine",
	compiler = {
		verbose = false,
		-- if you want to compile before loading init.fnl (recommended)
		hooks = { "oninit", "onsave" },
		-- if you only want after VimEnter event has fired
		-- hooks = { "onenter" },
	}
}



