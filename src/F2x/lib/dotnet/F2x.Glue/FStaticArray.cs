<<<<<<< Updated upstream
﻿using System;

namespace F2x.Glue
{
    /// <summary>
    /// This kind of array is used for stand-alone arrays (i.e. those that are not embedded into a derived type).
    /// </summary>
    /// <typeparam name="T">The type of the elements inside this array.</typeparam>
    public abstract class FStaticArray<T> : FArrayField<T>
    {

        /// <summary>
        /// Allocate a new Fortran array with the given size.
        /// </summary>
        /// <param name="size">The size of the new array for each dimension.</param>
        public FStaticArray(Int32[] size) : base(IntPtr.Zero, size)
        {
            this.ptr = this.Allocate(size);
            if (this.ptr != IntPtr.Zero)
            {
                this.size = size;
            }
            else
            {
                throw new InsufficientMemoryException();
            }
        }

        public FStaticArray(IntPtr ptr, Int32[] size) : base(ptr, size)
        {
        }

        /// <summary>
        /// The destuctor releases the allocated space. So be sure to keep a reference to the array as long as it is used.
        /// </summary>
        ~FStaticArray()
        {
            if (this.ptr != IntPtr.Zero)
            {
                this.Free();
                this.ptr = IntPtr.Zero;
                this.size = null;
            }
        }

        /// <summary>
        /// Allocate an array with the given size in Fortran. This method is exported from Fortran.
        /// </summary>
        /// <param name="size">Size of the array for each dimension.</param>
        /// <returns>A pointer to the newly allocated array.</returns>
        abstract protected IntPtr Allocate(Int32[] size);

        /// <summary>
        /// Release the memory allocated by Fortran. 
        /// </summary>
        abstract protected void Free();
    }
}
=======
﻿using System;

namespace F2x.Glue
{
    /// <summary>
    /// This kind of array is used for stand-alone arrays (i.e. those that are not embedded into a derived type).
    /// </summary>
    /// <typeparam name="T">The type of the elements inside this array.</typeparam>
    public abstract class FStaticArray<T> : FArrayField<T>
    {

        /// <summary>
        /// Allocate a new Fortran array with the given size.
        /// </summary>
        /// <param name="size">The size of the new array for each dimension.</param>
        public FStaticArray(Int32[] size) : base(IntPtr.Zero, size)
        {
            this.ptr = this.Allocate(size);
            if (this.ptr != IntPtr.Zero)
            {
                this.size = size;
            }
            else
            {
                throw new InsufficientMemoryException();
            }
        }

        public FStaticArray(IntPtr ptr, Int32[] size) : base(ptr, size)
        {
        }

        /// <summary>
        /// The destuctor releases the allocated space. So be sure to keep a reference to the array as long as it is used.
        /// </summary>
        ~FStaticArray()
        {
            if (this.ptr != IntPtr.Zero)
            {
                this.Free();
                this.ptr = IntPtr.Zero;
                this.size = null;
            }
        }

        /// <summary>
        /// Allocate an array with the given size in Fortran. This method is exported from Fortran.
        /// </summary>
        /// <param name="size">Size of the array for each dimension.</param>
        /// <returns>A pointer to the newly allocated array.</returns>
        abstract protected IntPtr Allocate(Int32[] size);

        /// <summary>
        /// Release the memory allocated by Fortran. 
        /// </summary>
        abstract protected void Free();
    }
}
>>>>>>> Stashed changes
