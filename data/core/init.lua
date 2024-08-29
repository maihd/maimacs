local fennel = require("libs.fennel")

table.insert(package.loaders or package.searchers, fennel.searcher)

fennel.path = EXEDIR .. '/data/?.fnl;' .. fennel.path
fennel.path = EXEDIR .. '/data/?/init.fnl;' .. fennel.path

fennel["macro-path"] = EXEDIR .. '/data/macros.fnl;' .. fennel["macro-path"]
fennel["macro-path"] = EXEDIR .. '/data/macros/init.fnl;' .. fennel["macro-path"]

-- table.insert(fennel["macro-searchers"], function (module_name)
--     local filename = fennel["search-module"](module_name, package.cpath)
--     if filename then
--         local func = EXEDIR .. '/data/?.fnl;' .. module_name
--         local func_init = EXEDIR .. '/data/?/init.fnl;' .. module_name
--         return function ()
--             return package.loadlib(filename, func) or package.loadlib(filename, func_init)
--         end, filename
--     end
-- end)

local fennel_state = fennel.install({
    allowedGlobals = true
})

-- Load Fennel bindings for native functions
fennel_state.dofile("data/binds/system.fnl")
fennel_state.dofile("data/binds/renderer.fnl")
fennel_state.dofile("data/binds/renderer-font.fnl")

-- Setup done, start the editor
-- with the logic from core.fnl
return fennel_state.dofile("data/core/core.fnl")
