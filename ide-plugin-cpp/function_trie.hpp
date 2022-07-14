#pragma once

#include <unordered_map>
#include <vector>

#include "function_data.hpp"

class Trie;

class Node
{
  std::vector<Function> data;
  std::string type;
  std::unordered_map<std::string, Node *> children;
  bool is_terminal;
  friend class Trie;

public:
  Node(std::string t)
  {
    type = t;
    is_terminal = false;
  }
};

class Trie
{
  Node *root;

public:
  Trie() { root = new Node(""); }
  // Insertion
  void insert(Function f)
  {
    Node *temp = root;
    for (auto type : f.argument_types)
    {
      if (temp->children.count(type) == 0)
      {
        Node *n = new Node(type);
        temp->children[type] = n;
      }
      temp = temp->children[type];
    }
    temp->data.push_back(f);
    temp->is_terminal = true;
  }

  // Search
  std::vector<Function> search(std::vector<std::string> arg_types)
  {
    Node *temp = root;
    std::vector<Function> result;
    // If no arguments passed, return only nullary functions (bad aproximation?)
    if (arg_types.empty())
    {
      result = root->data;
    }
    for (size_t i = 0; i < arg_types.size(); i++)
    {
      // If all of the remaining arguments are the same
      // you can add all the variadic functions with the last argument being of the same type
      if (std::all_of(arg_types.begin() + i, arg_types.end(),
                      [&](auto t)
                      { return t == arg_types[i]; }))
      {
        std::vector<Function> variadics;
        for (const auto &f : temp->data)
        {
          if (f.is_variadic && (f.argument_types.back() == arg_types[i]))
          {
            variadics.push_back(f);
          }
        }
        result.insert(result.end(), variadics.begin(), variadics.end());
      }
      if (temp->children.count(arg_types[i]) == 0)
      {
        std::sort(result.begin(), result.end());
        return result;
      }
      temp = temp->children[arg_types[i]];
    }
    // Add functions to result
    result.insert(result.end(), temp->data.begin(), temp->data.end());
    // All variadic functions of all the nested types
    // since it's possible that the provided arg list
    // has 0 args of the next type, matching variadic
    std::vector<Function> n_variadics{};
    for (const auto &c : temp->children)
    {
      for (const auto &f : c.second->data)
      {
        if (f.is_variadic)
        {
          n_variadics.push_back(f);
        }
      }
    }
    result.insert(result.end(), n_variadics.begin(), n_variadics.end());
    std::sort(result.begin(), result.end());
    return result;
  }
};
