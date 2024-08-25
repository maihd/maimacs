local fennel = require("libs.fennel")
table.insert(package.loaders or package.searchers, fennel.searcher)

-- Install Fennel to Lua runtime
fennel.install()

-- Bindings
fennel.dofile("data/binds/system.fnl")

return fennel.dofile("data/core/core.fnl")
