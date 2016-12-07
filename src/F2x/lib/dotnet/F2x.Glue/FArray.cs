using System;

namespace F2x.Glue
{
    /// <summary>
    /// General interface to access contents of an array.
    /// </summary>
    /// <typeparam name="T">Type of the elements this array contains.</typeparam>
    public interface FArrayAccess<T>
    {
        /// <summary>
        /// Get the item referenced by index.
        /// </summary>
        /// <param name="index">The indices for each dimension (i.e. length of index is number of dimensions). The translation between 0-based (C) and 1-based (Fortran) indices is done automatically.</param>
        /// <returns>The referenced item or value.</returns>
        T GetItem(Int32[] index);

        /// <summary>
        /// Copy data to managed array.
        /// </summary>
        /// <param name="dest">Target to copy to.</param>
        void CopyTo(T[] dest);

        /// <summary>
        /// Set the item referenced by index.
        /// </summary>
        /// <param name="index">The indices for each dimension (i.e. length of index is number of dimensions). The translation between 0-based (C) and 1-based (Fortran) indices is done automatically.</param>
        /// <param name="value">The value to set at the given index.</param>
        void SetItem(Int32[] index, T value);

        /// <summary>
        /// Copy data from managed array.
        /// </summary>
        /// <param name="source">Input data.</param>
        void CopyFrom(T[] source);
    }

    /// <summary>
    /// This is the base class for arrays wrapped by F2x using the "_glue.f90.t" and "_glue.cs.t" templates.
    /// </summary>
    /// <typeparam name="T">The type of the items contained by this array.</typeparam>
    public abstract class FArray<T>
    {
        /// <summary>
        /// Get accessor for the array
        /// </summary>
        protected abstract FArrayAccess<T> Access { get; }

        /// <summary>
        /// Number of array entries for each dimension.
        /// </summary>
        public abstract Int32[] Count { get; }

        /// <summary>
        /// Overall size of the array.
        /// </summary>
        public Int32 Size
        {
            get
            {
                Int32 size = 1;
                foreach (Int32 dim in this.Count)
                {
                    size *= dim;
                }
                return size;
            }
        }

        /// <summary>
        /// The rank (i.e. number of dimensions) this array holds.
        /// </summary>
        public Int32 Rank
        {
            get
            {
                return this.Count.Length;
            }
        }

        /// <summary>
        /// Array-like access to items.
        /// </summary>
        /// <param name="index">One index per dimension combined into an array. The translation between 0-based (C) and 1-based (Fortran) indices is done automatically.</param>
        public T this[Int32[] index]
        {
            get {
                this.CheckIndex(index);
                return this.Access.GetItem(index);
            }
            set {
                this.CheckIndex(index);
                this.Access.SetItem(index, value);
            }
        }

        /// <summary>
        /// Copy data to managed array. It will be written linearzed in Column-Major (i.e. Fortran).
        /// </summary>
        /// <param name="dest">Array to save data.</param>
        public void CopyTo(T[] dest)
        {
            this.Access.CopyTo(dest);
        }

        /// <summary>
        /// Copy data from managed array. It will be read linearized in Column-Major (i.e. Fortran).
        /// </summary>
        /// <param name="source">Data to copy in.</param>
        public void CopyFrom(T[] source)
        {
            this.Access.CopyFrom(source);
        }

        /// <summary>
        /// Test the index for validity. The length of the index (i.e. the rank) needs to match the rank of this array and each index
        /// needs to be within the respective bounds.
        /// </summary>
        /// <param name="index">The index that should be checked. The idnex to be checked should be 0-based.</param>
        /// <exception cref="RankException">The length of the index does not match the rank of the array.</exception>
        /// <exception cref="IndexOutOfRangeException">At least on of the index value is not within the array bounds.</exception>
        protected void CheckIndex(Int32[] index)
        {
            Int32[] count = this.Count;

            if (index.Length != count.Length)
            {
                throw new RankException(String.Format("The supplied index denotes rank {0} but the array has rank {1}.", index.Length, count.Length));
            }

            for (Int32 i = 0; i < index.Length; i++)
            {
                if (index[i] < 0 || index[i] >= count[i])
                {
                    throw new IndexOutOfRangeException(String.Format("In dimension {0}: {1} is no valid index for a count of {2}.", i, index[i], count[i]));
                }
            }
        }
    }
}
