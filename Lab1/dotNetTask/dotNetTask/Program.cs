using System;
using System.Collections.Generic;

namespace dotNetTask
{
    public class Program
    {
        static void Main(string[] args)
        {
            List<int> lst = functions.singleton(1);
            Console.WriteLine("Singleton:");
            foreach (var item in lst)
            {
                Console.WriteLine(item);
            }
            Console.WriteLine("Null: {0}", functions.Null(lst));
            lst = functions.snoc(lst, 2);
            Console.WriteLine("Snoc");
            foreach (var item in lst)
            {
                Console.WriteLine(item);
            }
            Console.WriteLine("Length: {0}", functions.Length(lst));
        }
    }
}