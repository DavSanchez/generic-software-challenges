# Function argument matcher

Suppose that we are developing some kind of IDE plugin with the following functionality. Provided there is a number of functions stored somewhere, described in the below form:

```cpp
struct Function {
  string name;
  vector<string> argumentTypes;
  bool isVariadic;
};
```

If a list of arguments is passed to this component, the functions that match these list of arguments will be retrieved. For example:

```cpp
register({
  funA:{["Boolean", "Integer"], isVariadic:false}, // Accepts 1 Bool and 1 Int
  funB:{["Integer"], isVariadic:false}, // accepts only 1 Int
  funC:{["Integer"], isVariadic:true}, // accepts 0 or more Ints
  funX:{["Boolean", "Integer"], isVariadic:true}, // Accepts 1 Bool and 0 or more Ints
 });

findMatches(["Boolean", "Integer"]);            // -> [funA, funX]
findMatches(["Integer"]);                       // -> [funB, funC]
findMatches(["Integer", "Integer", "Integer"]); // -> [funC]

// More Examples:

register({
  funD:{["String", "Integer", "Integer", "Integer"], isVariadic:true},
  funE:{["String", "Integer", "Integer"], isVariadic:false}
});

findMatches(["String", "Integer"])             // -> []
findMatches(["String", "Integer", "Integer"])  // -> [funD, funE] # funD due to supporting 0 variadics
findMatches(["String", "Integer", "Integer", "Integer", "Integer"])  // -> [funD]
```

## Description of the code

This implementation is based on tries. The entry point of the program is `trie_test.cpp`. It makes use of the `doctest` library for testing, so there is no `main` function defined (if you want to compile and run this, please download the [`doctest` header-only library](https://raw.githubusercontent.com/doctest/doctest/master/doctest/doctest.h) and place it in this directory). The data structures and methods used are located in the remaining C++ files: `function_data.cpp` and `function_trie.cpp`.

## Improvements

### Using `std::set` instead of `std::vector`.

One of the most obvious improvements is to switch the `std::vector<Function>` used as the data for each node of the trie with a `std::set<Function>`. This way we can avoid having repeated functions in the search results, while also avoiding the need to sort the results (see `function_trie.cpp:79` and `function_trie.cpp:101`) to pass the tests correctly, as `vector` tests for equality taking insertion order into account.
