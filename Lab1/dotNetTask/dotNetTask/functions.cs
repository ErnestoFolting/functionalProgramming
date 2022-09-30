using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace dotNetTask
{
    public static class functions
    {
        public static List<T> singleton<T>(T el)
        {
            return new() { el };
        } 
        
        public static bool Null<T>(List<T> elements)
        {
            return (Length(elements) == 0);
        }
        public static List<T> snoc <T> (List<T> lst, T el)
        {
            int previousSize = Length(lst);
            T[] newLst = new T[previousSize + 1];   
            for(int i = 0; i < previousSize; i++)
            {
                newLst[i] = lst[i];
            }
            newLst[previousSize] = el;
            return newLst.ToList();
        }
        public static int Length<T>(List<T> elements)
        {
            int count = 0;
            foreach (T element in elements)
            {
                count++;
            }
            return count;
        }
    }
}
