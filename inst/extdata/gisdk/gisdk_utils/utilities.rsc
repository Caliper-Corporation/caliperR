# © Copyright Caliper Corporation. Licensed under Apache License 2.0.

/*
Adds a field.
for the 12 potential elements.

  * view
    * String
    * view name
  * a_fields
    * Array of arrays
    * Each sub-array contains the 12-elements that describe a field.
      e.g. {{"Density", "Real", 10, 3, , , , "Used to calculate initial AT"}}
      (See ModifyTable() TC help page for full array info)
  * initial_values
    * Number, string, or array of numbers/strings (optional)
    * If not provided, any fields to add that already exist in the table will not be
      modified in any way. If provided, the added field will be set to this value.
      This can be used to ensure that a field is set to null, zero, etc. even if it
      already exists.
    * If a single value, it will be used for all fields.
    * If an array shorter than number of fields, the last value will be used
      for all remaining fields.
*/

Macro "Add Fields" (view, a_fields, initial_values)

  // Argument check
  if view = null then Throw("'view' not provided")
  if a_fields = null then Throw("'a_fields' not provided")
  for field in a_fields do
    if field = null then Throw("An element in the 'a_fields' array is missing")
  end
  if initial_values <> null then do
    if TypeOf(initial_values) <> "array" then initial_values = {initial_values}
    if TypeOf(initial_values) <> "array"
      then Throw("'initial_values' must be an array")
  end

  // Get current structure and preserve current fields by adding
  // current name to 12th array position
  a_str = GetTableStructure(view)
  for s = 1 to a_str.length do
    a_str[s] = a_str[s] + {a_str[s][1]}
  end
  for f = 1 to a_fields.length do
    a_field = a_fields[f]

    // Test if field already exists (will do nothing if so)
    field_name = a_field[1]
    exists = "False"
    for s = 1 to a_str.length do
      if a_str[s][1] = field_name then do
        exists = "True"
        break
      end
    end

    // If field does not exist, create it
    if !exists then do
      dim a_temp[12]
      for i = 1 to a_field.length do
        a_temp[i] = a_field[i]
      end
      a_str = a_str + {a_temp}
    end
  end

  ModifyTable(view, a_str)

  // Set initial field values if provided
  if initial_values <> null then do
    nrow = GetRecordCount(view, )
    for f = 1 to a_fields.length do
      field = a_fields[f][1]
      type = a_fields[f][2]
      if f > initial_values.length
        then init_value = initial_values[initial_values.length]
        else init_value = initial_values[f]

      if type = "Character" then type = "String"

      opts = null
      opts.Constant = init_value
      v = Vector(nrow, type, opts)
      SetDataVector(view + "|", field, v, )
    end
  end
EndMacro

/*
table   String Can be a file path or view of the table to modify
field   Array or string
string  Array or string
*/

Macro "Add Field Description" (table, field, description)

  if table = null or field = null or description = null then Throw(
    "Missing arguments to 'Add Field Description'"
    )
  if TypeOf(field) = "string" then field = {field}
  if TypeOf(description) = "string" then description = {description}
  if field.length <> description.length then Throw(
    "The same number of fields and descriptions must be provided."
  )
  isView = RunMacro("Is View", table)

  // If the table variable is not a view, then attempt to open it
  if isView = "no" then table = OpenTable("table", "FFB", {table})

  str = GetTableStructure(table)
  for f = 1 to str.length do
    str[f] = str[f] + {str[f][1]}
    name = str[f][1]

    pos = ArrayPosition(field, {name}, )
    if pos <> 0 then str[f][8] = description[pos]
  end
  ModifyTable(table, str)

  // If this macro opened the table, close it
  if isView = "no" then CloseView(table)
EndMacro

/*
Renames a field in a TC view

Inputs
  view_name
    String
    Name of view to modify

  current_name
    String
    Name of field to rename

  new_name
    String
    New name to use
*/

Macro "Rename Field" (view_name, current_name, new_name)

  // Argument Check
  if view_name = null then Throw("Rename Field: 'view_name' not provided")
  if current_name = null then Throw("Rename Field: 'current_name' not provided")
  if new_name = null then Throw("Rename Field: 'new_name' not provided")

  // Get and modify the field info array
  a_str = GetTableStructure(view_name)
  field_modified = "false"
  for s = 1 to a_str.length do
    a_field = a_str[s]
    field_name = a_field[1]

    // Add original field name to end of field array
    a_field = a_field + {field_name}

    // rename field if it's the current field
    if field_name = current_name then do
      a_field[1] = new_name
      field_modified = "true"
    end

    a_str[s] = a_field
  end

  // Modify the table
  ModifyTable(view_name, a_str)

  // Throw error if no field was modified
  if !field_modified
    then Throw(
      "Rename Field: Field '" + current_name +
      "' not found in view '" + view_name + "'"
    )
EndMacro

/*
Tests whether or not a string is a view name or not
*/

Macro "Is View" (string)

  a_views = GetViewNames()
  if ArrayPosition(a_views, {string}, ) = 0 then return("false")
  else return("true")
EndMacro

/*
Calling Exit directly from R does not work, so this is used.
*/

Macro "Exit"
  Exit()
endmacro

/*
Similar to Exit(), this must be called from a custom UI.
*/

Macro "GetProgram"
  return(GetProgram())
endmacro
