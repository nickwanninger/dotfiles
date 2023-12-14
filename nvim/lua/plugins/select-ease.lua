return {
  "ziontee113/SelectEase",
  config = function()
    -- local select_ease = require("SelectEase")
    --
    -- -- For more language support check the `Queries` section
    -- local lua_query = [[
    --         ;; query
    --         ((identifier) @cap)
    --         ((string_content) @cap)
    --         ((true) @cap)
    --         ((false) @cap)
    --     ]]
    -- local python_query = [[
    --         ;; query
    --         ((identifier) @cap)
    --         ((string) @cap)
    --     ]]
    --
    -- local c_query = [[
    --
    --       (call_expression
    --         arguments: (argument_list (_)? @cap))
    --       ;; ;; query
    --       ;; ((string_literal) @cap)
    --       ;; ((system_lib_string) @cap)
    --
    --       ;; ; Identifiers
    --       ;; ((identifier) @cap)
    --       ;; ((struct_specifier) @cap)
    --       ;; ((type_identifier) @cap)
    --       ;; ((sized_type_specifier) @cap)
    --       ;; ((field_identifier) @cap)
    --       ;; ((number_literal) @cap)
    --       ;; ((unary_expression) @cap)
    --       ;; ((pointer_declarator) @cap)
    --
    --       ;; ; Types
    --       ;; ((primitive_type) @cap)
    --
    --       ;; ; Expressions
    --       ;; (assignment_expression
    --       ;;   right: (_) @cap)
    --   ]]
    -- local cpp_query = [[
    --       ;; ((namespace_identifier) @cap)
    --   ]] .. c_query
    --
    -- local queries = {
    --   lua = lua_query,
    --   python = python_query,
    --   c = c_query,
    --   cpp = cpp_query,
    --   latex = [[
    --     ((label_definition) @cap)
    --   ]],
    --
    --   tex = [[
    --     ((label_definition) @cap)
    --   ]],
    -- }
    --
    -- vim.keymap.set({ "n", "s", "i" }, "<C-A-k>", function()
    --   select_ease.select_node({
    --     queries = queries,
    --     direction = "previous",
    --     vertical_drill_jump = true,
    --     -- visual_mode = true, -- if you want Visual Mode instead of Select Mode
    --     fallback = function()
    --       select_ease.select_node({ queries = queries, direction = "previous" })
    --     end,
    --   })
    -- end, {})
    --
    --
    -- vim.keymap.set({ "n", "s", "i" }, "<C-A-j>", function()
    --   select_ease.select_node({
    --     queries = queries,
    --     direction = "next",
    --     vertical_drill_jump = true,
    --     -- visual_mode = true, -- if you want Visual Mode instead of Select Mode
    --     fallback = function()
    --       select_ease.select_node({ queries = queries, direction = "next" })
    --     end,
    --   })
    -- end, {})
    --
  end,
}
