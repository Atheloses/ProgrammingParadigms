using System;
using System.Collections.Generic;
using System.Data;
using System.Linq;

namespace HW3
{
    class Program
    {
        static void Main(string[] args)
        {
            Dictionary<string, List<string>> grammar = new Dictionary<string, List<string>>();
            grammar.Add("E", new List<string> { "T E'" });
            grammar.Add("E'", new List<string> { "+ T E'", "eps" });
            grammar.Add("T", new List<string> { "F T'" });
            grammar.Add("T'", new List<string> { "* F T'", "eps" });
            grammar.Add("F", new List<string> { "( E )", "id" });

            Dictionary<string, List<string>> grammar3 = new Dictionary<string, List<string>>();
            grammar3.Add("E", new List<string> { "T E'" });
            grammar3.Add("E'", new List<string> { "+ T E'", "eps" });
            grammar3.Add("T", new List<string> { "F T'", "*" });
            grammar3.Add("T'", new List<string> { "* F T'", "eps" });
            grammar3.Add("F", new List<string> { "( E )", "id" });

            Dictionary<string, List<string>> grammar2 = new Dictionary<string, List<string>>();
            grammar2.Add("S", new List<string> { "( A )", "eps" });
            grammar2.Add("A", new List<string> { "T E" });
            grammar2.Add("E", new List<string> { "& T E", "eps" });
            grammar2.Add("T", new List<string> { "( A )", "a", "b", "c" });

            Dictionary<string, List<string>> grammar4 = new Dictionary<string, List<string>>();
            grammar4.Add("A", new List<string> { "b C", "B d" });
            grammar4.Add("B", new List<string> { "C C", "b A" });
            grammar4.Add("C", new List<string> { "c C", "eps" });

            Dictionary<string, List<string>> grammar5 = new Dictionary<string, List<string>>();
            grammar5.Add("S", new List<string> { "S * E", "E" });
            grammar5.Add("E", new List<string> { "T R" });
            grammar5.Add("R", new List<string> { "+ T R", "eps" });
            grammar5.Add("T", new List<string> { "( S )", "T ?", "i" });

            var useThisGram = grammar;
            Console.WriteLine("Grammar");
            Print(useThisGram, true);

            Console.WriteLine();
            Console.WriteLine("Empty NonTerminals");
            GetEmptyNonTerminals(useThisGram).ForEach(x => Console.Write(x + " "));
            Console.WriteLine();

            Console.WriteLine();
            Console.WriteLine("First");
            var firstSet = FirstSet(useThisGram);
            Print(firstSet, false);

            Console.WriteLine();
            Console.WriteLine("Follows");
            Print(FollowSet(useThisGram, firstSet), false);
        }

        static List<string> GetEmptyNonTerminals(Dictionary<string, List<string>> p_Grammar)
        {
            List<string> output = new List<string>();

            bool changed;
            do
            {
                changed = false;
                foreach (var row in p_Grammar.Where(x => !output.Contains(x.Key)))
                {
                    foreach (var right in row.Value)
                    {
                        var split = right.Split(' ');
                        var epsCount = 0;
                        for (int i = 0; i < split.Length; i++)
                            if (split[i] == "eps" || p_Grammar.ContainsKey(split[i]) && output.Contains(split[i]))
                                epsCount++;

                        if (epsCount == split.Count())
                        {
                            output.Add(row.Key);
                            changed = true;
                            break;
                        }
                    }
                }

            } while (changed);

            return output;
        }

        static Dictionary<string, List<string>> FirstSet(Dictionary<string, List<string>> p_Grammar)
        {
            Dictionary<string, List<string>> output = new Dictionary<string, List<string>>();

            foreach (var row in p_Grammar)
                output.Add(row.Key, FirstSetRec(row.Key, p_Grammar, null).ToHashSet().ToList());

            return output;
        }

        static List<string> FirstSetRec(string key, Dictionary<string, List<string>> p_Grammar, HashSet<string> p_History)
        {
            List<string> output = new List<string>();
            if (p_History == null)
                p_History = new HashSet<string>();

            if (!p_History.Contains(key))
            {
                p_History.Add(key);
                foreach (var value in p_Grammar[key])
                {
                    var split = value.Split(' ');

                    if (p_Grammar.ContainsKey(split[0]))
                        output.AddRange(FirstSetRec(split[0], p_Grammar, p_History));
                    else
                        output.Add(split[0]);
                }
            }

            return output;
        }

        static Dictionary<string, List<string>> FollowSet(Dictionary<string, List<string>> p_Grammar, Dictionary<string, List<string>> p_FirstSet)
        {
            Dictionary<string, List<string>> output = new Dictionary<string, List<string>>();
            foreach (var row in p_Grammar)
                output.Add(row.Key, new List<string>());

            output[p_Grammar.First().Key].Add("$");
            foreach (var row in p_Grammar)
            {
                foreach (var right in row.Value)
                {
                    var split = right.Split(' ');
                    for (int i = split.Length - 1; i > 0; i--)
                    {
                        if(output.ContainsKey(split[i]))
                        {
                            if (i < split.Length - 1 && split[i + 1] != "eps")
                            {
                                if (output.ContainsKey(split[i + 1]))
                                    output[split[i]].AddRange(p_FirstSet[split[i + 1]].Except(new List<string> { "eps" }));
                                else
                                    output[split[i]].Add(split[i + 1]);
                            }
                            if(i == split.Length - 1 || split[i + 1] == "eps" || output.ContainsKey(split[i+1]) && p_FirstSet[split[i + 1]].Contains("eps"))
                                output[split[i]].Add(row.Key);
                        }
                    }
                }
            }

            return FirstSet(output);
        }

        static void Print(Dictionary<string, List<string>> p_Grammar, bool p_Gram)
        {
            var maxLeft = p_Grammar.Keys.Select(x => x.Length).Max();

            foreach (var row in p_Grammar)
            {
                string rightSide = String.Empty;
                if (row.Value.Count > 0)
                {
                    if (p_Gram)
                    {
                        row.Value.ForEach(x => rightSide = rightSide + " " + x.ToString() + " |");
                        rightSide = rightSide.Remove(rightSide.Length - 2);
                    }
                    else
                    {
                        rightSide += " {";
                        row.Value.ForEach(x => rightSide = rightSide + " " + x.ToString() + " ,");
                        rightSide = rightSide.Remove(rightSide.Length - 1);
                        rightSide += "}";
                    }
                }
                else
                    rightSide = " ";


                Console.WriteLine(row.Key + new string(' ', maxLeft + 1 - row.Key.Length) + "->" + rightSide);
            }
        }
    }
}
