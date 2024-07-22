module SimpleFile;

external fopen: void.ptr(path: byte.ptr, mode: byte.ptr);
external fread: cusize(buffer: void.ptr, element_size: cint, element_count: cint, handle: void.ptr) discardable;
external fwrite: cusize(buffer: void.ptr, element_size: cint, element_count: cint, handle: void.ptr) discardable;
external fclose: void(handle: void.ptr);

external ftell: clong(handle: void.ptr);
external fseek: cint(handle: void.ptr, offset: clong, origin: cint) discardable;

export read: [byte](path: [byte view]) {
    handle :: fopen path.data, "rb"[0];

    SEEK_SET: cint 0;
    SEEK_CUR: cint 1;
    SEEK_END: cint 2;

    fseek handle 0 SEEK_END;
    size :: ftell handle;
    fseek handle 0 SEEK_SET;

    contents : [byte size];

    nread :: fread contents.data 1 (cint size) handle;
    contents.size := (u32 nread);

    fclose handle;

    return contents;
};
;; TODO: alias read to gstd_read;

export write: bool(path: [byte view], contents: [byte view]) discardable {
    handle :: fopen path.data "wb"[0];
    fwrite contents.data 1 (cint contents.size) handle;
    fclose handle;

    return true;
};
;; TODO: alias write to gstd_write;
