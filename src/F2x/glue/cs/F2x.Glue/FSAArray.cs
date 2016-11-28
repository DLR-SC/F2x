using System;

namespace F2x.Glue
{
    /// <summary>
    /// This kind of array is used for stand-alone arrays (i.e. those that are not embedded into a derived type).
    /// </summary>
    /// <typeparam name="T">The type of the elements inside this array.</typeparam>
    public abstract class FSAArray<T> : FArray<T>
    {
        /// <summary>
        /// The pointer to the Fortran array.
        /// </summary>
        protected IntPtr ptr;

        /// <summary>
        /// The size of this array for each dimension.
        /// </summary>
        protected Int32[] size;

        /// <summary>
        /// Allocate a new Fortran array with the given size.
        /// </summary>
        /// <param name="size">The size of the new array for each dimension.</param>
        public FSAArray(Int32[] size)
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

        /// <summary>
        /// The destuctor releases the allocated space. So be sure to keep a reference to the array as long as it is used.
        /// </summary>
        ~FSAArray()
        {
            if (this.ptr != IntPtr.Zero)
            {
                this.Free();
                this.ptr = IntPtr.Zero;
                this.size = null;
            }
        }

        /// <summary>
        /// Retrieve the Fortran pointer for the array.
        /// </summary>
        public IntPtr Ptr
        {
            get
            {
                return this.ptr;
            }
        }

        /// <see cref="FArray{T}.Count" />
        public override Int32[] Count
        {
            get
            {
                return this.size;
            }
        }

        /// <summary>
        /// Allocate an array with the given size in Fortran. This method is exported from Fortran.
        /// </summary>
        /// <param name="size">Size of the array for each dimension.</param>
        /// <returns>A pointer to the newly allocated array.</returns>
        protected abstract IntPtr Allocate(Int32[] size);

        /// <summary>
        /// Release the memory allocated by Fortran. 
        /// </summary>
        protected abstract void Free();
    }
}
