using System;
using System.Runtime.InteropServices;

namespace F2x.Glue
{
    /// <summary>
    /// This class provides a two-dimensional array with INTEGER values.
    /// </summary>
    public class FInt2DArray : FSAArray<Int32>
    {
        /// <summary>
        /// Create a new two-dimensional array.
        /// </summary>
        /// <param name="size">The sizes of the two dimensions.</param>
        public FInt2DArray(Int32[] size) : base(size)
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
        public FInt2DArray(Int32 i, Int32 j) : base(new Int32[] { i, j })
        {
        }

        /// <see cref="FSAArray{T}.Allocate(int[])" />
        protected override IntPtr Allocate(Int32[] size)
        {
            return FInt2DArray.AllocateArray(size);
        }

        /// <see cref="FSAArray{T}.Free()" />
        protected override void Free()
        {
            FInt2DArray.FreeArray(this.ptr, this.size);
        }

        /// <see cref="FArray{T}.GetItem(int[])" />
        public override Int32 GetItem(Int32[] index)
        {
            return FInt2DArray.GetArrayItem(this.ptr, this.size, index);
        }

        /// <see cref="FArray{T}.SetItem(int[], T)" />
        public override void SetItem(Int32[] index, Int32 value)
        {
            FInt2DArray.SetArrayItem(this.ptr, this.size, index, value);
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
                return this.GetItem(new Int32[] { i, j });
            }
            set
            {
                this.SetItem(new Int32[] { i, j }, value);
            }
        }

        #region External interface
        // The following extern functions represent the entry points into the Fortran lib.
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_alloc", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr AllocateArray([In] Int32[] size);
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_free", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr FreeArray(IntPtr ptr, [In] Int32[] size);
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_getitem", CallingConvention = CallingConvention.Cdecl)]
        private static extern Int32 GetArrayItem(IntPtr ptr, [In] Int32[] size, [In] Int32[] index);
        [DllImport("libF2x.so", EntryPoint = "F2x_int2d_setitem", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr SetArrayItem(IntPtr ptr, [In] Int32[] size, [In] Int32[] index, Int32 value);
        #endregion
    }

    /// <summary>
    /// This class provides a three-dimensional array with INTEGER values.
    /// </summary>
    public class FInt3DArray : FSAArray<Int32>
    {
        /// <summary>
        /// Create a new three-dimensional array.
        /// </summary>
        /// <param name="size">The sizes of the three dimensions.</param>
        public FInt3DArray(Int32[] size) : base(size)
        {
            if (size.Length != 3)
            {
                throw new RankException(String.Format("{0} dimensions given for rank 3.", size.Length));
            }
        }

        /// <summary>
        /// Simplified constructor to avoid manual creation of transient size array.
        /// </summary>
        /// <param name="i">Size of first dimension.</param>
        /// <param name="j">Size of second dimension.</param>
        /// <param name="k">Size of third dimension.</param>
        public FInt3DArray(Int32 i, Int32 j, Int32 k) : base(new Int32[] { i, j, k })
        {
        }

        /// <see cref="FSAArray{T}.Allocate(int[])" />
        protected override IntPtr Allocate(Int32[] size)
        {
            return FInt3DArray.AllocateArray(size);
        }

        /// <see cref="FSAArray{T}.Free()" />
        protected override void Free()
        {
            FInt3DArray.FreeArray(this.ptr, this.size);
        }

        /// <see cref="FArray{T}.GetItem(int[])" />
        public override Int32 GetItem(Int32[] index)
        {
            return FInt3DArray.GetArrayItem(this.ptr, this.size, index);
        }

        /// <see cref="FArray{T}.SetItem(int[], T)" />
        public override void SetItem(Int32[] index, Int32 value)
        {
            FInt3DArray.SetArrayItem(this.ptr, this.size, index, value);
        }

        /// <summary>
        /// Access three-dimensional arrays by three indices. This is a shorthand to avoid manual creation of transient array.
        /// </summary>
        /// <param name="i">First dimensions index, 0-based.</param>
        /// <param name="j">Section dimensions index, 0-based.</param>
        /// <param name="k">Thrid dimensions index, 0-based.</param>
        /// <returns>Referenced value.</returns>
        public Int32 this[Int32 i, Int32 j, Int32 k]
        {
            get
            {
                return this.GetItem(new Int32[] { i, j, k });
            }
            set
            {
                this.SetItem(new Int32[] { i, j, k }, value);
            }
        }

        #region External interface
        // The following extern functions represent the entry points into the Fortran lib.
        [DllImport("libF2x.so", EntryPoint = "F2x_int3d_alloc", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr AllocateArray([In] Int32[] size);
        [DllImport("libF2x.so", EntryPoint = "F2x_int3d_free", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr FreeArray(IntPtr ptr, [In] Int32[] size);
        [DllImport("libF2x.so", EntryPoint = "F2x_int3d_getitem", CallingConvention = CallingConvention.Cdecl)]
        private static extern Int32 GetArrayItem(IntPtr ptr, [In] Int32[] size, [In] Int32[] index);
        [DllImport("libF2x.so", EntryPoint = "F2x_int3d_setitem", CallingConvention = CallingConvention.Cdecl)]
        private static extern IntPtr SetArrayItem(IntPtr ptr, [In] Int32[] size, [In] Int32[] index, Int32 value);
        #endregion
    }
}
