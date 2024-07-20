module SimpleFile;

external fopen: void.ptr(path: Byte.ptr, mode: Byte.ptr);
external fread: cusize(buffer: void.ptr, element_size: cint, element_count: cint, handle: void.ptr) discardable;
external fwrite: cusize(buffer: void.ptr, element_size: cint, element_count: cint, handle: void.ptr) discardable;
external fclose: void(handle: void.ptr);

external ftell: clong(handle: void.ptr);
external fseek: cint(handle: void.ptr, offset: clong, origin: cint) discardable;

export gstd_read: [Byte](path: [Byte]) {
    handle :: fopen path.data, "rb"[0];

    ;; These are C macros, because of course they are.
    SEEK_SET: cint 0;
    SEEK_CUR: cint 1;
    SEEK_END: cint 2;

    fseek handle 0 SEEK_END;
    size :: ftell handle;
    fseek handle 0 SEEK_SET;

    contents : [Byte size];

    nread :: fread contents.data 1 (cint size) handle;
    contents.size := (u32 nread);

    fclose handle;

    return contents;
};

export gstd_write: Bool(path: [Byte], contents: [Byte]) discardable {
    handle :: fopen path.data "wb"[0];
    fwrite contents.data 1 (cint contents.size) handle;
    fclose handle;

    return true;
};
