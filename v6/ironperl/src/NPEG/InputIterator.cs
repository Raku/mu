#region License
/* **********************************************************************************
 * Copyright (c) Leblanc Meneses
 * This source code is subject to terms and conditions of the MIT License
 * for NPEG. A copy of the license can be found in the License.txt file
 * at the root of this distribution. 
 * By using this source code in any fashion, you are agreeing to be bound by the terms of the 
 * MIT License.
 * You must not remove this notice from this software.
 * **********************************************************************************/
#endregion
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace NPEG
{
    public class InputIterator
    {
        public InputIterator(String input)
        {
            this.Input = input;
            this.Index = 0;
            this.Length = input.Length;
        }

        public Int32 Index
        {
            get;
            set;
        }

        public Int32 Length
        {
            get;
            set;
        }

        private String Input
        {
            get;
            set;
        }


        public String GetText(Int32 start, Int32 end)
        {
            return this.Input.Substring(start, end - start);
        }


        public Char? LookAhead(Int32 tokenposition)
        {
            if (this.IsOutOfBounds(tokenposition))
            {
                throw new IteratorUsageException("Token argument does not exist in scanner's generated token collection.");
            }

            Int32 nexttokenindex = tokenposition + 1;
            return this.GetToken(nexttokenindex); 
        }

        public Char? LookBehind(Int32 tokenposition)
        {
            if (this.IsOutOfBounds(tokenposition))
            {
                throw new IteratorUsageException("Token argument does not exist in scanner's generated token collection.");
            }

            Int32 previoustokenindex = tokenposition - 1;
            return this.GetToken(previoustokenindex); 
        }





        // operations modify internal pointer
        public Char? Next()
        {
            this.Index += 1;
            return this.GetToken(this.Index); 
        }
        public Char? Previous() 
        {
            this.Index -= 1;
            return this.GetToken(this.Index); 
        }
 
        public Char? Current
        {
            get 
            {
                return this.GetToken(this.Index);                
            }
        }




        private Char? GetToken(Int32? index)
        {
            if ( this.IsOutOfBounds(index) )
            {
                return null;
            }
            else
            {
                return this.Input[(Int32)index];
            }
        }

        private Boolean IsOutOfBounds(Int32? index)
        {
            if (
                (this.Input.Length - 1) < index
                || 0 > index
                || index == null
            )
            {
                return true;
            }

            return false;
        }

    }











}
