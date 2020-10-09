namespace RegexpParser
{
  internal static class Matcher
  {
    public static bool Match(string regexp, string line)
    {
      var tree = TreeBuilder.Build(regexp);

      foreach (var symbol in line) 
        tree = tree.Derivative(symbol);

      return tree.Nullable;
    }
  }
}