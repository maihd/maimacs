local fennel = require("fennel")
local unpack = (table.unpack or _G.unpack)
local syntax = fennel.syntax()
local function line(ast)
  return fennel["ast-source"](ast).line
end
local function last_line_length(line0)
  return #line0:match("[^\n]*$")
end
local function any_3f(tbl, pred)
  local found = false
  for _, v in pairs(tbl) do
    if found then break end
    found = not not pred(v)
  end
  return found
end
local function strip_comments(t)
  local tbl_21_auto = {}
  local i_22_auto = 0
  for _, x in ipairs(t) do
    local val_23_auto
    if not fennel["comment?"](x) then
      val_23_auto = x
    else
      val_23_auto = nil
    end
    if (nil ~= val_23_auto) then
      i_22_auto = (i_22_auto + 1)
      tbl_21_auto[i_22_auto] = val_23_auto
    else
    end
  end
  return tbl_21_auto
end
local function view_fn_args(t, view, inspector, indent, start_indent, out)
  if fennel["sym?"](t[2]) then
    local third = view(t[3], inspector, (indent + 1))
    table.insert(out, " ")
    table.insert(out, third)
    if ("string" == type(t[4])) then
      table.insert(out, ("\n" .. string.rep(" ", start_indent)))
      inspector["escape-newlines?"] = false
      table.insert(out, view(t[4], inspector, start_indent))
      inspector["escape-newlines?"] = true
      return 5
    else
      return 4
    end
  else
    return 3
  end
end
local function first_thing_in_line_3f(out)
  local last = (out[#out] or "")
  return not last:match("[^\n]*$"):match("[^ ]")
end
local function break_pair_3f(let_3f, count, viewed, next_ast, next_next, indent)
  if let_3f then
    return ((1 == math.fmod(count, 2)) and not (fennel["comment?"](next_ast) and ((indent + 1 + last_line_length(viewed) + 1 + #tostring(next_ast)) <= 80)))
  else
    return ((("string" == type(next_ast)) or tostring(next_ast):find("^&")) and (80 < (indent + 1 + last_line_length(viewed) + 1 + #tostring(next_ast) + #fennel.view(next_next))))
  end
end
local function binding_comment(c, indent, out, start_indent)
  if ((80 < (indent + #tostring(c))) and out[#out]:match("^[^%s]")) then
    table.insert(out, ("\n" .. string.rep(" ", start_indent)))
  else
  end
  if (not first_thing_in_line_3f(out) and (#out ~= 1)) then
    table.insert(out, " ")
  else
  end
  table.insert(out, tostring(c))
  return table.insert(out, ("\n" .. string.rep(" ", start_indent)))
end
local function view_binding(bindings, view, inspector, start_indent, pair_wise_3f, open, close)
  local out = {open}
  local indent, offset, non_comment_count = start_indent, 0, 1
  for i = 1, #bindings do
    while fennel["comment?"](bindings[(i + offset)]) do
      binding_comment(bindings[(i + offset)], indent, out, start_indent)
      indent, offset = start_indent, (offset + 1)
    end
    local i0 = (offset + i)
    local viewed = view(bindings[i0], inspector, indent)
    if (i0 <= #bindings) then
      table.insert(out, viewed)
      non_comment_count = (non_comment_count + 1)
      if (i0 < #bindings) then
        if break_pair_3f(pair_wise_3f, non_comment_count, viewed, bindings[(i0 + 1)], bindings[(i0 + 2)], indent) then
          table.insert(out, ("\n" .. string.rep(" ", start_indent)))
          indent = start_indent
        else
          indent = (indent + 1 + last_line_length(viewed))
          table.insert(out, " ")
        end
      else
      end
    else
    end
  end
  table.insert(out, close)
  return table.concat(out)
end
local fn_forms = {fn = true, lambda = true, ["\206\187"] = true, macro = true}
local force_initial_newline = {["do"] = true, ["eval-compiler"] = true}
local function view_init_body(t, view, inspector, start_indent, out, callee)
  if force_initial_newline[callee] then
    table.insert(out, ("\n" .. string.rep(" ", start_indent)))
  elseif (nil ~= t[2]) then
    table.insert(out, " ")
  else
  end
  local indent
  if force_initial_newline[callee] then
    indent = start_indent
  else
    indent = (start_indent + #callee)
  end
  local second
  local _14_
  do
    local t_13_ = syntax
    if (nil ~= t_13_) then
      t_13_ = t_13_[callee]
    else
    end
    if (nil ~= t_13_) then
      t_13_ = t_13_["binding-form?"]
    else
    end
    _14_ = t_13_
  end
  if (_14_ and ("unquote" ~= tostring(t[2][1]))) then
    local function _17_()
      if fennel["list?"](t[2]) then
        return "(", ")"
      else
        return "[", "]"
      end
    end
    second = view_binding(t[2], view, inspector, (indent + 1), ("let" == callee), _17_())
  else
    second = view(t[2], inspector, indent)
  end
  local indent2 = (indent + #second:match("[^\n]*$"))
  if (nil ~= t[2]) then
    table.insert(out, second)
  else
  end
  if fn_forms[callee] then
    return view_fn_args(t, view, inspector, indent2, start_indent, out, callee)
  else
    return 3
  end
end
local function match_3f(callee)
  return ({match = true, case = true, ["match-try"] = true, ["case-try"] = true})[callee]
end
local function match_same_line_3f(callee, i, out, viewed, t)
  return (match_3f(callee) and (0 == math.fmod(i, 2)) and not any_3f(t, fennel["comment?"]) and (((string.find(viewed, "\n") or #viewed:match("[^\n]*$")) + 1 + last_line_length(out[#out])) <= 80))
end
local function trailing_comment_3f(viewed, body_indent)
  return (viewed:match("^; ") and (body_indent <= 80))
end
local one_element_per_line_forms = {["->"] = true, ["->>"] = true, ["-?>"] = true, ["-?>>"] = true, ["if"] = true}
local function space_out_fns_3f(prev, viewed, start_index, i)
  return (not (start_index == i) and (prev:match("^ *%(fn [^%[]") or viewed:match("^ *%(fn [^%[]")))
end
local function originally_same_lines_3f(t, n1, n2)
  local first = t[n1]
  local second = t[n2]
  local and_21_ = ("table" == type(second))
  if and_21_ then
    local _22_ = line(first)
    and_21_ = ((line(t) == _22_) and (_22_ == line(second)))
  end
  return and_21_
end
local function scalar_3f(form)
  return (fennel["sym?"](form) or fennel["comment?"](form) or fennel["varg?"](form) or ("table" ~= type(form)))
end
local function depth(form, base)
  if scalar_3f(form) then
    return base
  else
    local d = (base + 1)
    for _, elem in pairs(form) do
      d = math.max(d, depth(elem, (base + 1)))
    end
    return d
  end
end
local function preserve_same_line_3f(t, i, indent, out, viewed, depth0)
  return (((indent + #table.concat(out) + #viewed) <= 80) and (depth0 <= 3) and not fennel["comment?"](t[(i - 1)]) and ((("table" ~= type(t[i])) and (#t <= 4)) or (originally_same_lines_3f(t, 1, i) and not viewed:find("\n"))))
end
local function view_body(t, view, inspector, start_indent, out, callee)
  local start_index = view_init_body(t, view, inspector, start_indent, out, callee)
  local indent
  if one_element_per_line_forms[callee] then
    indent = (start_indent + #callee)
  else
    indent = start_indent
  end
  for i = (start_index or (#t + 1)), #t do
    local viewed = view(t[i], inspector, indent)
    local body_indent = (indent + 1 + last_line_length(out[#out]))
    if (match_same_line_3f(callee, i, out, viewed, t) or preserve_same_line_3f(t, i, indent, out, viewed, depth(t, 0)) or trailing_comment_3f(viewed, body_indent)) then
      table.insert(out, " ")
      table.insert(out, view(t[i], inspector, body_indent))
    else
      if space_out_fns_3f(out[#out], viewed, start_index, i) then
        table.insert(out, "\n")
      else
      end
      table.insert(out, ("\n" .. string.rep(" ", indent)))
      table.insert(out, viewed)
    end
  end
  return nil
end
local function line_exceeded_3f(inspector, indent, viewed)
  return (inspector["line-length"] < (indent + last_line_length(viewed)))
end
local function view_with_newline(view, inspector, out, t, i, start_indent)
  if (" " == out[#out]) then
    table.remove(out)
  else
  end
  table.insert(out, ("\n" .. string.rep(" ", start_indent)))
  local viewed = view(t[i], inspector, start_indent)
  table.insert(out, viewed)
  return (start_indent + #viewed:match("[^\n]*$"))
end
local function view_call(t, view, inspector, start_indent, out)
  local indent = start_indent
  for i = 2, #t do
    table.insert(out, " ")
    indent = (indent + 1)
    local viewed = view(t[i], inspector, (indent - 1))
    if (fennel["comment?"](t[(i - 1)]) or (line_exceeded_3f(inspector, indent, viewed) and (2 ~= i))) then
      indent = view_with_newline(view, inspector, out, t, i, start_indent)
    else
      table.insert(out, viewed)
      indent = (indent + #viewed:match("[^\n]*$"))
    end
  end
  return nil
end
local function view_pairwise_if(t, view, inspector, indent, out)
  return table.insert(out, (" " .. view_binding({select(2, unpack(t))}, view, inspector, indent, true, "", "")))
end
local function if_pair(view, a, b, c)
  local _29_
  if fennel["comment?"](c) then
    _29_ = (" " .. view(c))
  else
    _29_ = ""
  end
  return (view(a) .. " " .. view(b) .. _29_)
end
local function pairwise_if_3f(t, indent, i, view)
  if (#strip_comments(t) < 5) then
    return false
  elseif ("if" ~= tostring(t[1])) then
    return false
  elseif not t[i] then
    return true
  elseif (80 < (indent + 1 + #if_pair(view, select(i, unpack(t))))) then
    return false
  else
    local _31_
    if fennel.comment(t[(i + 2)]) then
      _31_ = (i + 3)
    else
      _31_ = (i + 2)
    end
    return pairwise_if_3f(t, indent, _31_, view)
  end
end
local function view_maybe_body(t, view, inspector, indent, start_indent, out, callee)
  if pairwise_if_3f(t, indent, 2, view) then
    return view_pairwise_if(t, view, inspector, indent, out)
  elseif not originally_same_lines_3f(t, 2, 3) then
    return view_body(t, view, inspector, (start_indent + 2), out, callee)
  else
    return view_call(t, view, inspector, indent, out, callee)
  end
end
local function newline_if_ends_in_comment(out, indent)
  if out[#out]:match("^ *;[^\n]*$") then
    return table.insert(out, ("\n" .. string.rep(" ", indent)))
  else
    return nil
  end
end
local sugars = {hashfn = "#", quote = "`", unquote = ","}
local function sweeten(t, view, inspector, indent)
  return (sugars[tostring(t[1])] .. view(t[2], inspector, (indent + 1)))
end
local maybe_body = {["->"] = true, ["->>"] = true, ["-?>"] = true, ["-?>>"] = true, doto = true, ["if"] = true}
local renames = {["#"] = "length", ["~="] = "not="}
local function body_form_3f(callee)
  local _37_
  do
    local t_36_ = syntax
    if (nil ~= t_36_) then
      t_36_ = t_36_[callee]
    else
    end
    if (nil ~= t_36_) then
      t_36_ = t_36_["body-form?"]
    else
    end
    _37_ = t_36_
  end
  return (_37_ or callee:find("%.with-") or callee:find("^with-") or callee:find("%.def") or callee:find("^def"))
end
local function view_list(t, view, inspector, start_indent)
  if sugars[tostring(t[1])] then
    return sweeten(t, view, inspector, start_indent)
  else
    local callee = view(t[1], inspector, (start_indent + 1))
    local callee0 = (renames[callee] or callee)
    local out = {"(", callee0}
    local indent
    if body_form_3f(callee0) then
      indent = (start_indent + 2)
    elseif callee0:find("\n") then
      indent = (last_line_length(callee0) + 1)
    else
      indent = (start_indent + #callee0 + 2)
    end
    if body_form_3f(callee0) then
      view_body(t, view, inspector, indent, out, callee0)
    elseif maybe_body[callee0] then
      view_maybe_body(t, view, inspector, indent, start_indent, out, callee0)
    else
      view_call(t, view, inspector, indent, out)
    end
    newline_if_ends_in_comment(out, indent)
    table.insert(out, ")")
    return table.concat(out)
  end
end
local slength
local _43_
do
  local tmp_3_auto = rawget(_G, "utf8")
  if (nil ~= tmp_3_auto) then
    _43_ = tmp_3_auto.len
  else
    _43_ = nil
  end
end
local or_45_ = _43_
if not or_45_ then
  local function _46_(_241)
    return #_241
  end
  or_45_ = _46_
end
slength = or_45_
local function maybe_attach_comment(x, indent, cs)
  if (cs and (0 < #cs)) then
    local _47_
    do
      local tbl_21_auto = {}
      local i_22_auto = 0
      for _, c in ipairs(cs) do
        local val_23_auto = tostring(c)
        if (nil ~= val_23_auto) then
          i_22_auto = (i_22_auto + 1)
          tbl_21_auto[i_22_auto] = val_23_auto
        else
        end
      end
      _47_ = tbl_21_auto
    end
    return (table.concat(_47_, ("\n" .. string.rep(" ", indent))) .. ("\n" .. string.rep(" ", indent)) .. x)
  else
    return x
  end
end
local function shorthand_pair_3f(k, v)
  return (("string" == type(k)) and fennel["sym?"](v) and (k == tostring(v)))
end
local function view_pair(t, view, inspector, indent, mt, key)
  local val = t[key]
  local k
  if shorthand_pair_3f(key, val) then
    k = ":"
  else
    k = view(key, inspector, (indent + 1), true)
  end
  local v = view(val, inspector, (indent + slength(k) + 1))
  local function _52_()
    local t_51_ = mt
    if (nil ~= t_51_) then
      t_51_ = t_51_.comments
    else
    end
    if (nil ~= t_51_) then
      t_51_ = t_51_.keys
    else
    end
    if (nil ~= t_51_) then
      t_51_ = t_51_[key]
    else
    end
    return t_51_
  end
  local function _57_()
    local t_56_ = mt
    if (nil ~= t_56_) then
      t_56_ = t_56_.comments
    else
    end
    if (nil ~= t_56_) then
      t_56_ = t_56_.values
    else
    end
    if (nil ~= t_56_) then
      t_56_ = t_56_[val]
    else
    end
    return t_56_
  end
  return (maybe_attach_comment(k, indent, _52_()) .. " " .. maybe_attach_comment(v, indent, _57_()))
end
local function view_multiline_kv(pair_strs, indent, last_comments)
  if (last_comments and (0 < #last_comments)) then
    for _, c in ipairs(last_comments) do
      table.insert(pair_strs, tostring(c))
    end
    table.insert(pair_strs, "}")
    return ("{" .. table.concat(pair_strs, ("\n" .. string.rep(" ", indent))))
  else
    return ("{" .. table.concat(pair_strs, ("\n" .. string.rep(" ", indent))) .. "}")
  end
end
local function sorter(a, b)
  if (type(a) == type(b)) then
    return (a < b)
  else
    return (tostring(a) < tostring(b))
  end
end
local function view_kv(t, view, inspector, indent)
  local indent0 = (indent + 1)
  local mt = getmetatable(t)
  local keys
  local or_63_ = mt.keys
  if not or_63_ then
    local tmp_9_auto
    do
      local tbl_21_auto = {}
      local i_22_auto = 0
      for k in pairs(t) do
        local val_23_auto = k
        if (nil ~= val_23_auto) then
          i_22_auto = (i_22_auto + 1)
          tbl_21_auto[i_22_auto] = val_23_auto
        else
        end
      end
      tmp_9_auto = tbl_21_auto
    end
    table.sort(tmp_9_auto, sorter)
    or_63_ = tmp_9_auto
  end
  keys = or_63_
  local pair_strs
  do
    local tbl_21_auto = {}
    local i_22_auto = 0
    for _, k in ipairs(keys) do
      local val_23_auto = view_pair(t, view, inspector, indent0, mt, k)
      if (nil ~= val_23_auto) then
        i_22_auto = (i_22_auto + 1)
        tbl_21_auto[i_22_auto] = val_23_auto
      else
      end
    end
    pair_strs = tbl_21_auto
  end
  local oneline = ("{" .. table.concat(pair_strs, " ") .. "}")
  local or_67_ = oneline:match("\n")
  if not or_67_ then
    local t_68_ = mt
    if (nil ~= t_68_) then
      t_68_ = t_68_.comments
    else
    end
    if (nil ~= t_68_) then
      t_68_ = t_68_.last
    else
    end
    if (nil ~= t_68_) then
      t_68_ = t_68_[1]
    else
    end
    or_67_ = t_68_
  end
  if (or_67_ or ((indent0 + #oneline) > inspector["line-length"])) then
    local function _73_()
      local t_72_ = mt
      if (nil ~= t_72_) then
        t_72_ = t_72_.comments
      else
      end
      if (nil ~= t_72_) then
        t_72_ = t_72_.last
      else
      end
      return t_72_
    end
    return view_multiline_kv(pair_strs, indent0, _73_())
  else
    return oneline
  end
end
local function walk_tree(root, f, custom_iterator)
  local function walk(iterfn, parent, idx, node)
    if f(idx, node, parent) then
      for k, v in iterfn(node) do
        walk(iterfn, node, k, v)
      end
      return nil
    else
      return nil
    end
  end
  walk((custom_iterator or pairs), nil, nil, root)
  return root
end
local function set_fennelview_metamethod(_idx, form)
  if not scalar_3f(form) then
    if (not fennel["list?"](form) and not fennel["sequence?"](form)) then
      local _78_ = getmetatable(form)
      if (nil ~= _78_) then
        local mt = _78_
        mt["__fennelview"] = view_kv
      else
        local _ = _78_
        setmetatable(form, {__fennelview = view_kv})
      end
    else
    end
    return true
  else
    return nil
  end
end
local function prefer_colon_3f(s)
  return (s:find("^[-%w?^_!$%&*+./|<=>]+$") and not s:find("^[-?^_!$%&*+./@|<=>%\\]+$"))
end
local function fnlfmt(ast)
  local _let_82_ = getmetatable(fennel.list())
  local list_mt = _let_82_
  local __fennelview = _let_82_["__fennelview"]
  local _
  list_mt.__fennelview = view_list
  _ = nil
  local _0 = walk_tree(ast, set_fennelview_metamethod)
  local ok_3f, val = pcall(fennel.view, ast, {["empty-as-sequence?"] = true, ["escape-newlines?"] = true, ["prefer-colon?"] = prefer_colon_3f})
  list_mt.__fennelview = __fennelview
  assert(ok_3f, val)
  return val
end
local function space_out_forms_3f(prev_ast, ast)
  return not (line(prev_ast) and line(ast) and (1 == (line(ast) - line(prev_ast))))
end
local function abort(filename)
  io.stderr:write(string.format("File not found: %s\n", filename))
  return os.exit(1)
end
local function format_file(filename, _83_)
  local no_comments = _83_["no-comments"]
  local f
  if (filename == "-") then
    f = io.stdin
  else
    local _ = filename
    f = (io.open(filename, "r") or abort(filename))
  end
  local original = f:read("*all")
  local parser = fennel.parser(fennel.stringStream(original), filename, {comments = not no_comments})
  local out = {}
  f:close()
  local skip_next_3f, prev_ast = false
  for ok_3f, ast in parser do
    assert(ok_3f, ast)
    if (skip_next_3f and ast.bytestart and ast.byteend) then
      table.insert(out, original:sub(ast.bytestart, ast.byteend))
      skip_next_3f = false
    elseif (fennel.comment(";; fnlfmt: skip") == ast) then
      skip_next_3f = true
      table.insert(out, "")
      table.insert(out, tostring(ast))
    else
      if (prev_ast and space_out_forms_3f(prev_ast, ast)) then
        table.insert(out, "")
      else
      end
      table.insert(out, fnlfmt(ast))
      skip_next_3f = false
    end
    prev_ast = ast
  end
  table.insert(out, "")
  local formatted = table.concat(out, "\n")
  return formatted, (formatted ~= original)
end
return {fnlfmt = fnlfmt, ["format-file"] = format_file, version = "0.3.2-dev"}
