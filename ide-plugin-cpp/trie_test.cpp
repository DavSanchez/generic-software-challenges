#include <iostream>

#include "function_data.hpp"
#include "function_trie.hpp"

#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include "doctest.h"

const Function funA(
    "funA",
    {"Bool", "Int"},
    false);

const Function funB(
    "funB",
    {"Int"},
    false);

const Function funC(
    "funC",
    {"Int"},
    true);

const Function funX(
    "funX",
    {"Bool", "Int"},
    true);

const Function funD(
    "funD",
    {"String", "Int", "Int", "Int"},
    true);

const Function funE(
    "funE",
    {"String", "Int", "Int"},
    false);

TEST_CASE("Test the function trie")
{
  Trie t;
  t.insert(funA);
  t.insert(funB);
  t.insert(funC);
  t.insert(funX);
  t.insert(funD);
  t.insert(funE);

  SUBCASE("Correctly returns a list of functions")
  {
    CHECK(t.search({"Bool", "Int"}) == std::vector<Function>{funA, funX});
    CHECK(t.search({"Int"}) == std::vector<Function>{funB, funC});
    CHECK(t.search({"Int", "Int", "Int"}) == std::vector<Function>{funC});
  }

  SUBCASE("Works with more examples")
  {
    CHECK(t.search({"String", "Integer"}) == std::vector<Function>{});
    CHECK(t.search({"String", "Int", "Int"}) == std::vector<Function>{funD, funE});
    CHECK(t.search({"String", "Int", "Int", "Int", "Int"}) == std::vector<Function>{funD});
  }
}
