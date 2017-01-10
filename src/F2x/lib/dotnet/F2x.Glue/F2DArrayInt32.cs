<<<<<<< Updated upstream
﻿using System;
using System.Runtime.InteropServices;

namespace F2x.Glue
{
    /// <summary>
    /// Helper class to access two-dimensional Fortran arrays.
    /// </summary>
    internal class F2DArrayInt32Access : FArrayAccess<Int32>
    {
        // Address of allocated array memory.
        private IntPtr ptr;

        // Sizes per dimension.
        private Int32[] size;

        /// <summary>
        /// Create a new two-dimensional integer array accessor.
        /// </summary>
        /// <param name="ptr">Address of allocated array memory.</param>
        /// <param name="size">Sizes per dimension.</param>
        public F2DArrayInt32Access(IntPtr ptr, Int32[] size)
        {
            this.ptr = ptr;
            this.size = size;
        }

        /// <see cref="FArrayAccess{T}.GetItem(int[])" />
        public Int32 GetItem(Int32[] index)
        {
            return F2DArrayInt32Access.GetArrayItem(this.ptr, this.size, index);
        }

        /// <see cref="FArrayAccess{T}.CopyTo(T[])" />
        public void CopyTo(Int32[] dest)
        {
            Int32 size = Math.Min(this.size[0] * this.size[1], dest.Length);
            for (Int32 i = 0; i < this.size[0]; i++)
            {
                for (Int32 j = 0; j < this.size[1]; j++)
                {
                    Int32 index = i * this.size[1] + j;

                    if (index >= size)
                    {
                        break;
                    }

                    dest[index] = this.GetItem(new Int32[] { i, j });
                }
            }
        }

        /// <see cref="FArrayAccess{T}.SetItem(int[], T)" />
        public void SetItem(Int32[] index, Int32 value)
        {
            F2DArrayInt32Access.SetArrayItem(this.ptr, this.size, index, value);
        }

        /// <see cref="FArrayAccess{T}.CopyFrom(T[])" />
        public void CopyFrom(Int32[] source)
        {
            Int32 size = Math.Min(this.size[0] * this.size[1], source.Length);
            for (Int32 i = 0; i < this.size[0]; i++)
            {
                for (Int32 j = 0; j < this.size[1]; j++)
                {
                    Int32 index = i * this.size[1] + j;

                    if (index >= size)
                    {
                        break;
                    }

                    this.SetItem(new Int32[] { i, j }, source[index]);
                }
            }
        }

        /// <summary>
        /// Allocate new memory for a two-dimensional array.
        /// </summary>
        /// <param name="size">The sizes for the two dimensions.</param>
        /// <returns>The address of the newly allocated memory.</returns>
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_alloc", CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr AllocateArray([In] Int32[] size);

        /// <summary>
        /// Free memory that was allocated to an array.
        /// </summary>
        /// <param name="ptr">Address of the allocated memory.</param>
        /// <param name="size">Sizes of the two dimensions.</param>
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_free", CallingConvention = CallingConvention.Cdecl)]
        public static extern void FreeArray(IntPtr ptr, [In] Int32[] size);

        /// <summary>
        /// Access a certain item in the array.
        /// </summary>
        /// <param name="ptr">Address of the memory allocated to the array.</param>
        /// <param name="size">Sizes of the array for each of the two dimensions.</param>
        /// <param name="index">The two indices identifying the item.</param>
        /// <returns>The value currently stored in the array.</returns>
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_getitem", CallingConvention = CallingConvention.Cdecl)]
        public static extern Int32 GetArrayItem(IntPtr ptr, [In] Int32[] size, [In] Int32[] index);

        /// <summary>
        /// Set a certain item to a new value.
        /// </summary>
        /// <param name="ptr">Address of the memory allocated to the array.</param>
        /// <param name="size">Sizes of the array for each of the two dimensions.</param>
        /// <param name="index">The two indices identifying the item.</param>
        /// <param name="value">The new value to be stored.</param>
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_setitem", CallingConvention = CallingConvention.Cdecl)]
        public static extern void SetArrayItem(IntPtr ptr, [In] Int32[] size, [In] Int32[] index, Int32 value);
    }


    public class F2DArrayFieldInt32 : FArrayField<Int32>
    {
        // Address of the owner of this field.
        protected IntPtr owner;

        // Cache to access manager.
        private F2DArrayInt32Access access;

        /// <summary>
        /// Access a two-dimensional integer array stored in a derived type.
        /// </summary>
        /// <param name="owner"></param>
        /// <param name="size"></param>
        /// <param name="getter"></param>
        public F2DArrayFieldInt32(IntPtr owner, Int32[] size, FTypeGetArray getter) : base(getter(owner), size)
        {
            this.owner = owner;
            this.access = new F2DArrayInt32Access(this.ptr, size);
        }

        /// <see cref="FArray{T}.Access" />
        protected override FArrayAccess<Int32> Access
        {
            get
            {
                return this.access;
            }
        }

        /// <summary>
        /// Access two-dimensional arrays by two indices. This is a shorthand to avoid manual creation of transient array.
        /// </summary>
        /// <param name="i">First dimensions index, 0-based.</param>
        /// <param name="j">Section dimensions index, 0-based.</param>
        /// <returns>Referenced value.</returns>
        public Int32 this[Int32 i, Int32 j]
        {
            get
            {
                return this[new Int32[] { i, j }];
            }
            set
            {
                this[new Int32[] { i, j }] = value;
            }
        }
    }

    /// <summary>
    /// This class provides a two-dimensional array with INTEGER values.
    /// </summary>
    public class F2DArrayInt32 : FStaticArray<Int32>
    {
        private FArrayAccess<Int32> access;

        /// <summary>
        /// Create a new two-dimensional array.
        /// </summary>
        /// <param name="size">The sizes of the two dimensions.</param>
        public F2DArrayInt32(Int32[] size) : base(size)
        {
            if (size.Length != 2)
            {
                throw new RankException(String.Format("{0} dimensions given for rank 2.", size.Length));
            }
        }

        /// <summary>
        /// Simplified constructor to avoid manual creation of transient size array.
        /// </summary>
        /// <param name="i">Size of first dimension.</param>
        /// <param name="j">Size of second dimension.</param>
        public F2DArrayInt32(Int32 i, Int32 j) : base(new Int32[] { i, j })
        {
        }

        /// <see cref="FStaticArray{T}.Allocate(int[])" />
        protected override IntPtr Allocate(Int32[] size)
        {
            this.access = null;
            return F2DArrayInt32Access.AllocateArray(size);
        }

        /// <see cref="FStaticArray{T}.Free()" />
        protected override void Free()
        {
            this.access = null;
            F2DArrayInt32Access.FreeArray(this.ptr, this.size);
        }

        /// <see cref="FArray{T}.Access" />
        protected override FArrayAccess<int> Access
        {
            get
            {
                if (this.access == null)
                {
                    this.access = new F2DArrayInt32Access(this.ptr, size);
                }

                return this.access;
            }
        }

        /// <summary>
        /// Access two-dimensional arrays by two indices. This is a shorthand to avoid manual creation of transient array.
        /// </summary>
        /// <param name="i">First dimensions index, 0-based.</param>
        /// <param name="j">Section dimensions index, 0-based.</param>
        /// <returns>Referenced value.</returns>
        public Int32 this[Int32 i, Int32 j]
        {
            get
            {
                return this[new Int32[] { i, j }];
            }

            set
            {
                this[new Int32[] { i, j }] = value;
            }
        }
    }
}
=======
﻿using System;
using System.Runtime.InteropServices;

namespace F2x.Glue
{
    /// <summary>
    /// Helper class to access two-dimensional Fortran arrays.
    /// </summary>
    internal class F2DArrayInt32Access : FArrayAccess<Int32>
    {
        // Address of allocated array memory.
        private IntPtr ptr;

        // Sizes per dimension.
        private Int32[] size;

        /// <summary>
        /// Create a new two-dimensional integer array accessor.
        /// </summary>
        /// <param name="ptr">Address of allocated array memory.</param>
        /// <param name="size">Sizes per dimension.</param>
        public F2DArrayInt32Access(IntPtr ptr, Int32[] size)
        {
            this.ptr = ptr;
            this.size = size;
        }

        /// <see cref="FArrayAccess{T}.GetItem(int[])" />
        public Int32 GetItem(Int32[] index)
        {
            return F2DArrayInt32Access.GetArrayItem(this.ptr, this.size, index);
        }

        /// <see cref="FArrayAccess{T}.CopyTo(T[])" />
        public void CopyTo(Int32[] dest)
        {
            Int32 size = Math.Min(this.size[0] * this.size[1], dest.Length);
            for (Int32 i = 0; i < this.size[0]; i++)
            {
                for (Int32 j = 0; j < this.size[1]; j++)
                {
                    Int32 index = i * this.size[1] + j;

                    if (index >= size)
                    {
                        break;
                    }

                    dest[index] = this.GetItem(new Int32[] { i, j });
                }
            }
        }

        /// <see cref="FArrayAccess{T}.SetItem(int[], T)" />
        public void SetItem(Int32[] index, Int32 value)
        {
            F2DArrayInt32Access.SetArrayItem(this.ptr, this.size, index, value);
        }

        /// <see cref="FArrayAccess{T}.CopyFrom(T[])" />
        public void CopyFrom(Int32[] source)
        {
            Int32 size = Math.Min(this.size[0] * this.size[1], source.Length);
            for (Int32 i = 0; i < this.size[0]; i++)
            {
                for (Int32 j = 0; j < this.size[1]; j++)
                {
                    Int32 index = i * this.size[1] + j;

                    if (index >= size)
                    {
                        break;
                    }

                    this.SetItem(new Int32[] { i, j }, source[index]);
                }
            }
        }

        /// <summary>
        /// Allocate new memory for a two-dimensional array.
        /// </summary>
        /// <param name="size">The sizes for the two dimensions.</param>
        /// <returns>The address of the newly allocated memory.</returns>
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_alloc", CallingConvention = CallingConvention.Cdecl)]
        public static extern IntPtr AllocateArray([In] Int32[] size);

        /// <summary>
        /// Free memory that was allocated to an array.
        /// </summary>
        /// <param name="ptr">Address of the allocated memory.</param>
        /// <param name="size">Sizes of the two dimensions.</param>
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_free", CallingConvention = CallingConvention.Cdecl)]
        public static extern void FreeArray(IntPtr ptr, [In] Int32[] size);

        /// <summary>
        /// Access a certain item in the array.
        /// </summary>
        /// <param name="ptr">Address of the memory allocated to the array.</param>
        /// <param name="size">Sizes of the array for each of the two dimensions.</param>
        /// <param name="index">The two indices identifying the item.</param>
        /// <returns>The value currently stored in the array.</returns>
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_getitem", CallingConvention = CallingConvention.Cdecl)]
        public static extern Int32 GetArrayItem(IntPtr ptr, [In] Int32[] size, [In] Int32[] index);

        /// <summary>
        /// Set a certain item to a new value.
        /// </summary>
        /// <param name="ptr">Address of the memory allocated to the array.</param>
        /// <param name="size">Sizes of the array for each of the two dimensions.</param>
        /// <param name="index">The two indices identifying the item.</param>
        /// <param name="value">The new value to be stored.</param>
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_setitem", CallingConvention = CallingConvention.Cdecl)]
        public static extern void SetArrayItem(IntPtr ptr, [In] Int32[] size, [In] Int32[] index, Int32 value);
    }


    public class F2DArrayFieldInt32 : FArrayField<Int32>
    {
        // Address of the owner of this field.
        protected IntPtr owner;

        // Cache to access manager.
        private F2DArrayInt32Access access;

        /// <summary>
        /// Access a two-dimensional integer array stored in a derived type.
        /// </summary>
        /// <param name="owner"></param>
        /// <param name="size"></param>
        /// <param name="getter"></param>
        public F2DArrayFieldInt32(IntPtr owner, Int32[] size, FTypeGetArray getter) : base(getter(owner), size)
        {
            this.owner = owner;
            this.access = new F2DArrayInt32Access(this.ptr, size);
        }

        /// <see cref="FArray{T}.Access" />
        protected override FArrayAccess<Int32> Access
        {
            get
            {
                return this.access;
            }
        }

        /// <summary>
        /// Access two-dimensional arrays by two indices. This is a shorthand to avoid manual creation of transient array.
        /// </summary>
        /// <param name="i">First dimensions index, 0-based.</param>
        /// <param name="j">Section dimensions index, 0-based.</param>
        /// <returns>Referenced value.</returns>
        public Int32 this[Int32 i, Int32 j]
        {
            get
            {
                return this[new Int32[] { i, j }];
            }
            set
            {
                this[new Int32[] { i, j }] = value;
            }
        }
    }

    /// <summary>
    /// This class provides a two-dimensional array with INTEGER values.
    /// </summary>
    public class F2DArrayInt32 : FStaticArray<Int32>
    {
        private FArrayAccess<Int32> access;

        /// <summary>
        /// Create a new two-dimensional array.
        /// </summary>
        /// <param name="size">The sizes of the two dimensions.</param>
        public F2DArrayInt32(Int32[] size) : base(size)
        {
            if (size.Length != 2)
            {
                throw new RankException(String.Format("{0} dimensions given for rank 2.", size.Length));
            }
        }

        /// <summary>
        /// Simplified constructor to avoid manual creation of transient size array.
        /// </summary>
        /// <param name="i">Size of first dimension.</param>
        /// <param name="j">Size of second dimension.</param>
        public F2DArrayInt32(Int32 i, Int32 j) : base(new Int32[] { i, j })
        {
        }

        /// <see cref="FStaticArray{T}.Allocate(int[])" />
        protected override IntPtr Allocate(Int32[] size)
        {
            this.access = null;
            return F2DArrayInt32Access.AllocateArray(size);
        }

        /// <see cref="FStaticArray{T}.Free()" />
        protected override void Free()
        {
            this.access = null;
            F2DArrayInt32Access.FreeArray(this.ptr, this.size);
        }

        /// <see cref="FArray{T}.Access" />
        protected override FArrayAccess<int> Access
        {
            get
            {
                if (this.access == null)
                {
                    this.access = new F2DArrayInt32Access(this.ptr, size);
                }

                return this.access;
            }
        }

        /// <summary>
        /// Access two-dimensional arrays by two indices. This is a shorthand to avoid manual creation of transient array.
        /// </summary>
        /// <param name="i">First dimensions index, 0-based.</param>
        /// <param name="j">Section dimensions index, 0-based.</param>
        /// <returns>Referenced value.</returns>
        public Int32 this[Int32 i, Int32 j]
        {
            get
            {
                return this[new Int32[] { i, j }];
            }

            set
            {
                this[new Int32[] { i, j }] = value;
            }
        }
    }
}
>>>>>>> Stashed changes
