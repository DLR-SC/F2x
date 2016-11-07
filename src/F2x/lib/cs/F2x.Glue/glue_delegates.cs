namespace F2x.Glue {
	using System;

	public delegate void InitFunc(IntPtr ptr, Int32 size);
	public delegate void ClearFunc(IntPtr ptr);
	public delegate Int32 GetSizeFunc(IntPtr ptr);
	public delegate IntPtr GetAddrFunc(IntPtr ptr);
	public delegate IntPtr GetItemFunc(IntPtr ptr, Int32 index);
	
}

