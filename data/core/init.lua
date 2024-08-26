local fennel = require("libs.fennel")

-- Install Fennel to Lua runtime
-- fennel.install()

table.insert(package.loaders or package.searchers, fennel.searcher)

fennel.path = EXEDIR .. '/data/?.fnl;' .. fennel.path
fennel.path = EXEDIR .. '/data/?/init.fnl;' .. fennel.path

table.insert(fennel["macro-searchers"], function (module_name)
    local filename = fennel["search-module"](module_name, package.cpath)
    if filename then
        local func = EXEDIR .. '/data/?.fnl;' .. module_name
        local func_init = EXEDIR .. '/data/?/init.fnl;' .. module_name
        return function ()
            return package.loadlib(filename, func) or package.loadlib(filename, func_init)
        end, filename
    end
end)

-- Bindings
fennel.dofile("data/binds/system.fnl")

return fennel.dofile("data/core/core.fnl")
