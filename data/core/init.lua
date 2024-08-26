local fennel = require("libs.fennel")

table.insert(package.loaders or package.searchers, fennel.searcher)

fennel.path = EXEDIR .. '/data/?.fnl;' .. fennel.path
fennel.path = EXEDIR .. '/data/?/init.fnl;' .. fennel.path

fennel["macro-path"] = EXEDIR .. '/data/?.fnl;' .. fennel["macro-path"]
fennel["macro-path"] = EXEDIR .. '/data/?/init.fnl;' .. fennel["macro-path"]

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

return fennel.install().dofile("data/core/core.fnl")
