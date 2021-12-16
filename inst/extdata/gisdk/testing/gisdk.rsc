// © Copyright Caliper Corporation. Licensed under Apache License 2.0.

// Most basic
Macro "first macro"
    return("Hello World!")
endmacro

// With basic arguments
Macro "add" (int1, int2)
    return(int1 + int2)
endmacro

// With a simple array argument
Macro "first element" (array)
    return(array[1])
endmacro

// Use this to see the structure of an array
Macro "show array" (array)
    ShowArray(array)
endmacro

// Use this to verify option array parsing
Macro "parse opts array" (opts)
    first_name = opts[1][1]
    first_value = opts.(first_name)
    return(
        "The first option name is " + first_name + ". " +
        "The first option value is " + String(first_value) + "."
    )
endmacro

// R can understand a simple array returned
Macro "return array"
    return({1, 2})
endmacro

// R returns a list of lists
Macro "return nested array"
    return({{1, 2}, {3, 4}})
endmacro

// R returns a named list
Macro "return named array"
    return({{"one", 1}, {"two", 2}})
endmacro

// R does not understand a returned vector, but the caliperR package
// does the conversion.
Macro "return vector"
    v = A2V({1, 2})
    return(v)
endmacro

// Used for a unit test
Macro "test nested vector"
    return({test: A2V({1, 2, null})})
endmacro

// This is used to test how long it takes data to make a round trip over COM.
Macro "round trip" (arg)
    return(arg)
endmacro
