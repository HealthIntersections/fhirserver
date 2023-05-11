// source: https://github.com/libgit2/GitForDelphi/blob/master/uGitForDelphi.pas
// MIT License

unit uGitForDelphi;

interface

uses
   SysUtils, Windows;

function InitLibgit2: Boolean;

const
//   /** Size (in bytes) of a raw/binary oid */
//   #define GIT_OID_RAWSZ 20
   GIT_OID_RAWSZ = 20;
//   /** Size (in bytes) of a hex formatted oid */
//   #define GIT_OID_HEXSZ (GIT_OID_RAWSZ * 2)
   GIT_OID_HEXSZ = (GIT_OID_RAWSZ * 2);
//   #define GIT_OID_MINPREFIXLEN 4
   GIT_OID_MINPREFIXLEN = 4;
//   #define GIT_PACK_NAME_MAX (5 + 40 + 1)
   GIT_PACK_NAME_MAX = (5 + 40 + 1);

   // git_otype, enum with integer values
   //      typedef enum {
   //      GIT_OBJ_ANY = -2,    /**< Object can be any of the following */
   //      GIT_OBJ_BAD = -1,       /**< Object is invalid. */
   //      GIT_OBJ__EXT1 = 0,      /**< Reserved for future use. */
   //      GIT_OBJ_COMMIT = 1,     /**< A commit object. */
   //      GIT_OBJ_TREE = 2,       /**< A tree (directory listing) object. */
   //      GIT_OBJ_BLOB = 3,       /**< A file revision object. */
   //      GIT_OBJ_TAG = 4,        /**< An annotated tag object. */
   //      GIT_OBJ__EXT2 = 5,      /**< Reserved for future use. */
   //      GIT_OBJ_OFS_DELTA = 6,  /**< A delta, base is given by an offset. */
   //      GIT_OBJ_REF_DELTA = 7,  /**< A delta, base is given by object id. */
   //   } git_otype;
   GIT_OBJ_ANY = -2;
   GIT_OBJ_BAD = -1;
   GIT_OBJ__EXT1 = 0;
   GIT_OBJ_COMMIT = 1;
   GIT_OBJ_TREE = 2;
   GIT_OBJ_BLOB = 3;
   GIT_OBJ_TAG = 4;
   GIT_OBJ__EXT2 = 5;
   GIT_OBJ_OFS_DELTA = 6;
   GIT_OBJ_REF_DELTA = 7;

//   typedef enum {
//      GIT_SUCCESS = 0,
//      GIT_ERROR = -1,
//
//      /** Input was not a properly formatted Git object id. */
//      GIT_ENOTOID = -2,
//
//      /** Input does not exist in the scope searched. */
//      GIT_ENOTFOUND = -3,
//
//      /** Not enough space available. */
//      GIT_ENOMEM = -4,
//
//      /** Consult the OS error information. */
//      GIT_EOSERR = -5,
//
//      /** The specified object is of invalid type */
//      GIT_EOBJTYPE = -6,
//
//      /** The specified repository is invalid */
//      GIT_ENOTAREPO = -7,
//
//      /** The object type is invalid or doesn't match */
//      GIT_EINVALIDTYPE = -8,
//
//      /** The object cannot be written because it's missing internal data */
//      GIT_EMISSINGOBJDATA = -9,
//
//      /** The packfile for the ODB is corrupted */
//      GIT_EPACKCORRUPTED = -10,
//
//      /** Failed to acquire or release a file lock */
//      GIT_EFLOCKFAIL = -11,
//
//      /** The Z library failed to inflate/deflate an object's data */
//      GIT_EZLIB = -12,
//
//      /** The queried object is currently busy */
//      GIT_EBUSY = -13,
//
//      /** The index file is not backed up by an existing repository */
//      GIT_EBAREINDEX = -14,
//
//      /** The name of the reference is not valid */
//      GIT_EINVALIDREFNAME = -15,
//
//      /** The specified reference has its data corrupted */
//      GIT_EREFCORRUPTED  = -16,
//
//      /** The specified symbolic reference is too deeply nested */
//      GIT_ETOONESTEDSYMREF = -17,
//
//      /** The pack-refs file is either corrupted or its format is not currently supported */
//      GIT_EPACKEDREFSCORRUPTED = -18,
//
//      /** The path is invalid */
//      GIT_EINVALIDPATH = -19,
//
//      /** The revision walker is empty; there are no more commits left to iterate */
//      GIT_EREVWALKOVER = -20,
//
//      /** The state of the reference is not valid */
//      GIT_EINVALIDREFSTATE = -21,
//
//      /** This feature has not been implemented yet */
//      GIT_ENOTIMPLEMENTED = -22,
//
//      /** A reference with this name already exists */
//      GIT_EEXISTS = -23,
//
//      /** The given integer literal is too large to be parsed */
//      GIT_EOVERFLOW = -24,
//
//      /** The given literal is not a valid number */
//      GIT_ENOTNUM = -25,
//
//      /** Streaming error */
//      GIT_ESTREAM = -26,
//
//      /** invalid arguments to function */
//      GIT_EINVALIDARGS = -27,
//
//      /** The specified object has its data corrupted */
//      GIT_EOBJCORRUPTED = -28,
//
//      /** The given short oid is ambiguous */
//      GIT_EAMBIGUOUSOIDPREFIX = -29,
//
//      /** Skip and passthrough the given ODB backend */
//      GIT_EPASSTHROUGH = -30,
//
//      /** The path pattern and string did not match */
//      GIT_ENOMATCH = -31,
//
//      /** The buffer is too short to satisfy the request */
//      GIT_ESHORTBUFFER = -32,
//   } git_error;
   GIT_SUCCESS                = 0;
   GIT_ERROR                  = -1;
   GIT_ENOTOID                = -2;
   GIT_ENOTFOUND              = -3;
   GIT_ENOMEM                 = -4;
   GIT_EOSERR                 = -5;
   GIT_EOBJTYPE               = -6;
   GIT_ENOTAREPO              = -7;
   GIT_EINVALIDTYPE           = -8;
   GIT_EMISSINGOBJDATA        = -9;
   GIT_EPACKCORRUPTED         = -10;
   GIT_EFLOCKFAIL             = -11;
   GIT_EZLIB                  = -12;
   GIT_EBUSY                  = -13;
   GIT_EBAREINDEX             = -14;
   GIT_EINVALIDREFNAME        = -15;
   GIT_EREFCORRUPTED          = -16;
   GIT_ETOONESTEDSYMREF       = -17;
   GIT_EPACKEDREFSCORRUPTED   = -18;
   GIT_EINVALIDPATH           = -19;
   GIT_EREVWALKOVER           = -20;
   GIT_EINVALIDREFSTATE       = -21;
   GIT_ENOTIMPLEMENTED        = -22;
   GIT_EEXISTS                = -23;
   GIT_EOVERFLOW              = -24;
   GIT_ENOTNUM                = -25;
   GIT_ESTREAM                = -26;
   GIT_EINVALIDARGS           = -27;
   GIT_EOBJCORRUPTED          = -28;
   GIT_EAMBIGUOUSOIDPREFIX    = -29;
   GIT_EPASSTHROUGH           = -30;
   GIT_ENOMATCH               = -31;
   GIT_ESHORTBUFFER           = -32;

//
//   /**
//    * Sort the repository contents in no particular ordering;
//    * this sorting is arbitrary, implementation-specific
//    * and subject to change at any time.
//    * This is the default sorting for new walkers.
//    */
//   #define GIT_SORT_NONE         (0)
//
//   /**
//    * Sort the repository contents in topological order
//    * (parents before children); this sorting mode
//    * can be combined with time sorting.
//    */
//   #define GIT_SORT_TOPOLOGICAL  (1 << 0)
//
//   /**
//    * Sort the repository contents by commit time;
//    * this sorting mode can be combined with
//    * topological sorting.
//    */
//   #define GIT_SORT_TIME         (1 << 1)
//
//   /**
//    * Iterate through the repository contents in reverse
//    * order; this sorting mode can be combined with
//    * any of the above.
//    */
//   #define GIT_SORT_REVERSE      (1 << 2)
   GIT_SORT_NONE        = (0);
   GIT_SORT_TOPOLOGICAL = (1 shl 0);
   GIT_SORT_TIME        = (1 shl 1);
   GIT_SORT_REVERSE     = (1 shl 2);

//   /** Basic type of any Git reference. */
//   typedef enum {
//      GIT_REF_INVALID = 0, /** Invalid reference */
//      GIT_REF_OID = 1, /** A reference which points at an object id */
//      GIT_REF_SYMBOLIC = 2, /** A reference which points at another reference */
//      GIT_REF_PACKED = 4,
//      GIT_REF_HAS_PEEL = 8,
//      GIT_REF_LISTALL = GIT_REF_OID|GIT_REF_SYMBOLIC|GIT_REF_PACKED,
//   } git_rtype

   GIT_REF_INVALID   = 0;
   GIT_REF_OID       = 1;
   GIT_REF_SYMBOLIC  = 2;
   GIT_REF_PACKED    = 4;
   GIT_REF_HAS_PEEL  = 8;
   GIT_REF_LISTALL   = GIT_REF_OID or GIT_REF_SYMBOLIC or GIT_REF_PACKED;

//      typedef enum {
//      GIT_STREAM_RDONLY = (1 << 1),
//      GIT_STREAM_WRONLY = (1 << 2),
//      GIT_STREAM_RW = (GIT_STREAM_RDONLY | GIT_STREAM_WRONLY),
//   } git_odb_streammode;

   GIT_STREAM_RDONLY = (1 shl 1);
   GIT_STREAM_WRONLY = (1 shl 2);
   GIT_STREAM_RW     = (GIT_STREAM_RDONLY or GIT_STREAM_WRONLY);

//   #define GIT_IDXENTRY_UPDATE            (1 << 0)
//   #define GIT_IDXENTRY_REMOVE            (1 << 1)
//   #define GIT_IDXENTRY_UPTODATE          (1 << 2)
//   #define GIT_IDXENTRY_ADDED             (1 << 3)
//
//   #define GIT_IDXENTRY_HASHED            (1 << 4)
//   #define GIT_IDXENTRY_UNHASHED          (1 << 5)
//   #define GIT_IDXENTRY_WT_REMOVE         (1 << 6) /* remove in work directory */
//   #define GIT_IDXENTRY_CONFLICTED        (1 << 7)
//
//   #define GIT_IDXENTRY_UNPACKED          (1 << 8)
//   #define GIT_IDXENTRY_NEW_SKIP_WORKTREE (1 << 9)
//
//   /*
//    * Extended on-disk flags:
//    */
//   #define GIT_IDXENTRY_INTENT_TO_ADD     (1 << 13)
//   #define GIT_IDXENTRY_SKIP_WORKTREE     (1 << 14)
//   /* GIT_IDXENTRY_EXTENDED2 is for future extension */
//   #define GIT_IDXENTRY_EXTENDED2         (1 << 15)
//
//   #define GIT_IDXENTRY_EXTENDED_FLAGS (GIT_IDXENTRY_INTENT_TO_ADD | GIT_IDXENTRY_SKIP_WORKTREE)
   GIT_IDXENTRY_UPDATE              = (1 shl 0);
   GIT_IDXENTRY_REMOVE              = (1 shl 1);
   GIT_IDXENTRY_UPTODATE            = (1 shl 2);
   GIT_IDXENTRY_ADDED               = (1 shl 3);

   GIT_IDXENTRY_HASHED              = (1 shl 4);
   GIT_IDXENTRY_UNHASHED            = (1 shl 5);
   GIT_IDXENTRY_WT_REMOVE           = (1 shl 6); //* remove in work directory */
   GIT_IDXENTRY_CONFLICTED          = (1 shl 7);

   GIT_IDXENTRY_UNPACKED            = (1 shl 8);
   GIT_IDXENTRY_NEW_SKIP_WORKTREE   = (1 shl 9);

// * Extended on-disk flags:
   GIT_IDXENTRY_INTENT_TO_ADD       = (1 shl 13);
   GIT_IDXENTRY_SKIP_WORKTREE       = (1 shl 14);
//* GIT_IDXENTRY_EXTENDED2 is for future extension */
   GIT_IDXENTRY_EXTENDED2           = (1 shl 15);

   GIT_IDXENTRY_EXTENDED_FLAGS      = (GIT_IDXENTRY_INTENT_TO_ADD or GIT_IDXENTRY_SKIP_WORKTREE);


   GIT_STATUS_CURRENT               = 0;

   //** Flags for index status */
   GIT_STATUS_INDEX_NEW             = (1 shl 0);
   GIT_STATUS_INDEX_MODIFIED        = (1 shl 1);
   GIT_STATUS_INDEX_DELETED         = (1 shl 2);

   //** Flags for worktree status */
   GIT_STATUS_WT_NEW                = (1 shl 3);
   GIT_STATUS_WT_MODIFIED           = (1 shl 4);
   GIT_STATUS_WT_DELETED            = (1 shl 5);

   GIT_STATUS_IGNORED               = (1 shl 6);


   GIT_HEAD_FILE:       PAnsiChar = 'HEAD';
   GIT_MERGE_HEAD_FILE: PAnsiChar = 'MERGE_HEAD';
   GIT_DIR:             PAnsiChar = '.git/';
   GIT_INDEX_FILE:      PAnsiChar = 'index';
   GIT_OBJECTS_DIR:     PAnsiChar = 'objects/';
   GIT_REFS_HEADS_DIR:  PAnsiChar = 'heads/';
   GIT_PACKEDREFS_FILE: PAnsiChar = 'packed-refs';
   GIT_REFS_TAGS_DIR:   PAnsiChar = 'refs/tags/';

   GIT_DIR_FETCH                    = 0;
   GIT_DIR_PUSH                     = 1;

type
   size_t   = LongWord;
   time_t   = Int64;
   off_t = Int64;
   git_off_t = Int64;  //  typedef __int64 git_off_t;
   git_time_t = Int64; //  typedef __time64_t git_time_t;
   PInt32_t = ^Int32;
   PInt64_t = PInt64;

   git_otype            = Integer; // enum as constants above
   git_rtype            = Integer;
   git_odb_streammode   = Integer;

   git_file = Integer;

//   typedef struct {
//      /** raw binary formatted id */
//      unsigned char id[GIT_OID_RAWSZ];
//   } git_oid;
   git_oid = record
      id:                                                array[0..GIT_OID_RAWSZ-1] of Byte;
   end;

   PPByte = ^PByte;
   Pgit_oid = ^git_oid;
   PPgit_oid = ^Pgit_oid;
   PPgit_odb = ^Pgit_odb;
   PPgit_commit = ^Pgit_commit;
   PPgit_index_entry = ^Pgit_index_entry;
   Pgit_index_entry = ^git_index_entry;
   PPgit_index_tree = ^Pgit_index_tree;
   Pgit_signature = ^git_signature;
   PPgit_tree_entry = ^Pgit_tree_entry;
   Pgit_odb_backend = ^git_odb_backend;
   Pgit_strarray = ^git_strarray;
   Pgit_index_entry_unmerged = ^git_index_entry_unmerged;
   Pgit_config_file  = ^git_config_file;

   // structs not translated because they should be internal details,
   // and not necessary from GitForDelphi
   Pgit_odb          = Pointer;
   Pgit_commit       = Pointer;
   Pgit_index        = Pointer;
   Pgit_index_tree   = Pointer;
   Pgit_object       = Pointer;
   Pgit_tree         = Pointer;
   Pgit_tag          = Pointer;
   Pgit_blob         = Pointer;
   Pgit_reference    = Pointer;
   Pgit_repository   = Pointer;
   Pgit_tree_entry   = Pointer;
   Pgit_rawobj       = Pointer;
   Pgit_odb_object   = Pointer;
   Pgit_odb_stream   = Pointer;
   Pgit_revwalk      = Pointer;
   Pgit_treebuilder  = Pointer;
   Pgit_config       = Pointer;
   Pgit_indexer      = Pointer;
   Pgit_indexer_stats= Pointer;
   Pgit_reflog       = Pointer;
   Pgit_reflog_entry = Pointer;
   Pgit_remote       = Pointer;
   Pgit_refspec      = Pointer;

//   typedef int (*git_vector_cmp)(const void *, const void *);
   git_vector_cmp = Pointer;

   // int (*callback)(const char *, void *)
   git_reference_foreach_callback = function (const name: PAnsiChar; payload: PByte): Integer; stdcall;
   Pgit_reference_foreach_callback = ^git_reference_foreach_callback;

   // int (*callback)(const char *, unsigned int, void *)
   git_status_foreach_callback = function (const name: PAnsiChar; flags: UInt; payload: PByte): Integer; stdcall;
   Pgit_status_foreach_callback = ^git_status_foreach_callback;

   // typedef int (*git_treewalk_cb)(const char *root, git_tree_entry *entry, void *payload);
   git_treewalk_cb = function(root: PAnsiChar; entry: Pgit_tree_entry; payload: PByte): Integer; stdcall;
   Pgit_treewalk_cb = ^git_treewalk_cb;

//   struct git_remote_head {
//      int local:1; /* available locally */
//      git_oid oid;
//      git_oid loid;
//      char *name;
//   };
   git_remote_head = record
      local:                                             Integer;
      oid:                                               git_oid;
      loid:                                              git_oid;
      name:                                              PAnsiChar;
   end;
   Pgit_remote_head = ^git_remote_head;

   // typedef int (*git_headlist_cb)(git_remote_head *, void *);
   git_headlist_cb = function (head: Pgit_remote_head; payload: PByte): Integer; stdcall;
   Pgit_headlist_cb = ^git_headlist_cb;

//   struct git_odb_backend {
//      git_odb *odb;
//
//      int (* read)(
//            void **, size_t *, git_otype *,
//            struct git_odb_backend *,
//            const git_oid *);
//
//      /* To find a unique object given a prefix
//       * of its oid.
//       * The oid given must be so that the
//       * remaining (GIT_OID_HEXSZ - len)*4 bits
//       * are 0s.
//       */
//      int (* read_prefix)(
//            git_oid *,
//            void **, size_t *, git_otype *,
//            struct git_odb_backend *,
//            const git_oid *,
//            unsigned int);
//
//      int (* read_header)(
//            size_t *, git_otype *,
//            struct git_odb_backend *,
//            const git_oid *);
//
//      int (* write)(
//            git_oid *,
//            struct git_odb_backend *,
//            const void *,
//            size_t,
//            git_otype);
//
//      int (* writestream)(
//            struct git_odb_stream **,
//            struct git_odb_backend *,
//            size_t,
//            git_otype);
//
//      int (* readstream)(
//            struct git_odb_stream **,
//            struct git_odb_backend *,
//            const git_oid *);
//
//      int (* exists)(
//            struct git_odb_backend *,
//            const git_oid *);
//
//      void (* free)(struct git_odb_backend *);
//   };
   git_odb_backend_read          = function (var buffer_p: PByte; var len_p: size_t; var type_p: git_otype; backend: Pgit_odb_backend; const oid: Pgit_oid ): Integer; stdcall;
   git_odb_backend_read_prefix   = function (id: Pgit_oid; var buffer_p: PByte; var len_p: size_t; var type_p: git_otype; backend: Pgit_odb_backend; const short_oid: Pgit_oid; len: UInt): Integer; stdcall;
   git_odb_backend_read_header   = function (var len_p: size_t; var type_p: git_otype; backend: Pgit_odb_backend; const oid: Pgit_oid ): Integer; stdcall;
   git_odb_backend_write         = function (id: Pgit_oid; backend: Pgit_odb_backend; const data: PByte; len: size_t; type_: git_otype): Integer; stdcall;
   git_odb_backend_writestream   = function (var stream_out: Pgit_odb_stream; backend: Pgit_odb_backend; length: size_t; type_: git_otype): Integer; stdcall;
   git_odb_backend_readstream    = function (var stream_out: Pgit_odb_stream; backend: Pgit_odb_backend; const oid: Pgit_oid): Integer; stdcall;
   git_odb_backend_exists        = function (backend: Pgit_odb_backend; const oid: Pgit_oid): Integer; stdcall;
   git_odb_backend_free          = procedure (backend: Pgit_odb_backend); stdcall;

   git_odb_backend = record
      odb:                                               Pgit_odb;

      read:                                              ^git_odb_backend_read;
      read_prefix:                                       ^git_odb_backend_read_prefix;
      read_header:                                       ^git_odb_backend_read_header;
      write:                                             ^git_odb_backend_write;
      writestream:                                       ^git_odb_backend_writestream;
      readstream:                                        ^git_odb_backend_readstream;
      exists:                                            ^git_odb_backend_exists;
      free:                                              ^git_odb_backend_free;
   end;

//   typedef struct {
//      git_time_t seconds;
//      /* nsec should not be stored as time_t compatible */
//      unsigned int nanoseconds;
//   } git_index_time;
   git_index_time = record
      seconds:                                           git_time_t;
      nanoseconds:                                       UInt;
   end;

//   typedef struct git_index_entry {
//      git_index_time ctime;
//      git_index_time mtime;
//
//      unsigned int dev;
//      unsigned int ino;
//      unsigned int mode;
//      unsigned int uid;
//      unsigned int gid;
//      git_off_t file_size;
//
//      git_oid oid;
//
//      unsigned short flags;
//      unsigned short flags_extended;
//
//      const char *path;
//   } git_index_entry;
   git_index_entry = record
      ctime:                                             git_index_time;
      mtime:                                             git_index_time;

      dev:                                               UInt;
      ino:                                               UInt;
      mode:                                              UInt;
      uid:                                               UInt;
      gid:                                               UInt;
      file_size:                                         git_off_t;

      oid:                                               git_oid;

      flags:                                             SHORT;
      flags_extended:                                    SHORT;

      path:                                              PAnsiChar;
   end;

//   typedef struct git_index_entry_unmerged {
//      unsigned int mode[3];
//      git_oid oid[3];
//      char *path;
//   } git_index_entry_unmerged;
   git_index_entry_unmerged = record
      mode:                                              array [0..2] of UInt;
      oid:                                               array [0..2] of git_oid;
      path:                                              PAnsiChar;
   end;

//   typedef struct {
//      volatile int val;
//   } git_atomic;
   git_atomic = record
      val:                                               Integer;
   end;

//   typedef struct {
//      git_oid oid;
//      git_atomic refcount;
//   } git_cached_obj;
   git_cached_obj = record
      oid:                                               git_oid;
      refcount:                                          git_atomic;
   end;

//   /** Time in a signature */
//   typedef struct git_time {
//      time_t time; /** time in seconds from epoch */
//      int offset; /** timezone offset, in minutes */
//   } git_time;
   git_time = record
      time:                                              time_t;
      offset:                                            Integer;
   end;

//   /** An action signature (e.g. for committers, taggers, etc) */
//   typedef struct git_signature {
//      char *name; /** full name of the author */
//      char *email; /** email of the author */
//      git_time when; /** time when the action happened */
//   } git_signature;
   git_signature = record
      name:                                              PAnsiChar;
      email:                                             PAnsiChar;
      when:                                              git_time;
   end;

//   typedef struct {
//      char **strings;
//      size_t count;
//   } git_strarray;
   git_strarray = record
      strings:                                           PPAnsiChar;
      count:                                             size_t;
   end;

   Pgit_treebuilder_filter_filter = ^git_treebuilder_filter_filter;
   git_treebuilder_filter_filter = function (const entry: Pgit_tree_entry; payload: PByte): Integer; stdcall;

   Pgit_config_file_foreach_callback = ^git_config_file_foreach_callback;
   git_config_file_foreach_callback = function (const key, value: PAnsiChar; data: PByte): Integer; stdcall;

   Pgit_config_file_open = ^git_config_file_open;
   Pgit_config_file_get = ^git_config_file_get;
   Pgit_config_file_set = ^git_config_file_set;
   Pgit_config_file_del = ^git_config_file_del;
   Pgit_config_file_foreach = ^git_config_file_foreach;
   Pgit_config_file_free = ^git_config_file_free;

   git_config_file_open    = function (f: Pgit_config_file): Integer; stdcall;
   git_config_file_get     = function (f: Pgit_config_file; const key: PAnsiChar; out value: PAnsiChar): Integer; stdcall;
   git_config_file_set     = function (f: Pgit_config_file; const key, value: PAnsiChar): Integer; stdcall;
   git_config_file_del     = function (f: Pgit_config_file; const key: PAnsiChar): Integer; stdcall;
   git_config_file_foreach = function (f: Pgit_config_file; fn: Pgit_config_file_foreach_callback; data: PByte): Integer; stdcall;
   git_config_file_free    = function (f: Pgit_config_file): Integer; stdcall;

//   struct git_config_file {
//      struct git_config *cfg;
//
//      /* Open means open the file/database and parse if necessary */
//      int (*open)(struct git_config_file *);
//      int (*get)(struct git_config_file *, const char *key, const char **value);
//      int (*set)(struct git_config_file *, const char *key, const char *value);
//      int (*del)(struct git_config_file *, const char *key);
//      int (*foreach)(struct git_config_file *, int (*fn)(const char *, void *), void *data);
//      void (*free)(struct git_config_file *);
//   };
   git_config_file = record
      cfg:                                               Pgit_config;

      open:                                              Pgit_config_file_open;
      get:                                               Pgit_config_file_get;
      set_:                                              Pgit_config_file_set;
      del:                                               Pgit_config_file_del;
      foreach:                                           Pgit_config_file_foreach;
      free:                                              Pgit_config_file_free;
   end;

//   int (*callback)(const char *var_name, const char *value, void *payload)
   Pgit_config_foreach_callback = ^git_config_foreach_callback;
   git_config_foreach_callback = function (const key, value: PAnsiChar; payload: PByte): Integer; stdcall;

//   int (*callback)(const char *name, const char *value, void *payload)
   Pgit_attr_foreach_callback = ^git_attr_foreach_callback;
   git_attr_foreach_callback = function (name, value: PAnsiChar; payload: PByte): Integer; stdcall;

//   typedef enum {
//      GIT_REPO_PATH,
//      GIT_REPO_PATH_INDEX,
//      GIT_REPO_PATH_ODB,
//      GIT_REPO_PATH_WORKDIR
//   } git_repository_pathid;
const
   GIT_REPO_PATH           = 0;
   GIT_REPO_PATH_INDEX     = 1;
   GIT_REPO_PATH_ODB       = 2;
   GIT_REPO_PATH_WORKDIR   = 3;
type
   git_repository_pathid = Integer;


var
   /// attr.h
   ///   Attribute management

//   GIT_EXTERN(const char *)git_attr__true;
   git_attr__true:                     function (): PAnsiChar; stdcall;
//   GIT_EXTERN(const char *)git_attr__false;
   git_attr__false:                    function (): PAnsiChar; stdcall;

//   GIT_EXTERN(int) git_attr_get(git_repository *repo, const char *path, const char *name, const char **value);
   git_attr_get:                       function (repo: Pgit_repository; path, name: PAnsiChar; var value: PAnsiChar): Integer; stdcall;
//   GIT_EXTERN(int) git_attr_get_many(git_repository *repo, const char *path, size_t num_attr, const char **names, const char **values);
   git_attr_get_many:                  function (repo: Pgit_repository; path: PAnsiChar; num_attr: size_t; names: PPAnsiChar; var values: PAnsiChar): Integer; stdcall;
//   GIT_EXTERN(int) git_attr_foreach(git_repository *repo, const char *path, int (*callback)(const char *name, const char *value, void *payload), void *payload);
   git_attr_foreach:                   function (repo: Pgit_repository; path: PAnsiChar; callback: Pgit_attr_foreach_callback; payload: PByte): Integer; stdcall;
//   GIT_EXTERN(void) git_attr_cache_flush(git_repository *repo);
   git_attr_cache_flush:               procedure (repo: Pgit_repository); stdcall;
//   GIT_EXTERN(int) git_attr_add_macro(git_repository *repo, const char *name, const char *values);
   git_attr_add_macro:                 function (repo: Pgit_repository; name, values: PAnsiChar): Integer; stdcall;

   /// blob.h
   ///

   // GIT_EXTERN(const void *) git_blob_rawcontent(git_blob *blob);
   git_blob_rawcontent:                function (blob: Pgit_blob): PByte; stdcall;
   // GIT_EXTERN(size_t) git_blob_rawsize(git_blob *blob);
   git_blob_rawsize:                   function (blob: Pgit_blob): size_t; stdcall;
   // GIT_EXTERN(int) git_blob_create_fromfile(git_oid *oid, git_repository *repo, const char *path);
   git_blob_create_fromfile:           function (oid: Pgit_oid; repo: Pgit_repository; const path: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_blob_create_frombuffer(git_oid *oid, git_repository *repo, const void *buffer, size_t len);
   git_blob_create_frombuffer:         function (oid: Pgit_oid; repo: Pgit_repository; const buffer: PByte; len: size_t): Integer; stdcall;

   /// commit.h
   ///

   // GIT_EXTERN(const git_oid *) git_commit_id(git_commit *commit);
   git_commit_id:                      function (commit: Pgit_commit): Pgit_oid; stdcall;
//   GIT_EXTERN(const char *) git_commit_message_encoding(git_commit *commit);
   git_commit_message_encoding:        function (commit: Pgit_commit): PAnsiChar; stdcall;
   // GIT_EXTERN(const char *) git_commit_message(git_commit *commit);
   git_commit_message:                 function (commit: Pgit_commit): PAnsiChar; stdcall;
   // GIT_EXTERN(git_time_t) git_commit_time(git_commit *commit);
   git_commit_time:                    function (commit: Pgit_commit): git_time_t; stdcall;
   // GIT_EXTERN(int) git_commit_time_offset(git_commit *commit);
   git_commit_time_offset:             function (commit: Pgit_commit): Integer; stdcall;
   // GIT_EXTERN(const git_signature *) git_commit_committer(git_commit *commit);
   git_commit_committer:               function (commit: Pgit_commit): Pgit_signature; stdcall;
   // GIT_EXTERN(const git_signature *) git_commit_author(git_commit *commit);
   git_commit_author:                  function (commit: Pgit_commit): Pgit_signature; stdcall;
   // GIT_EXTERN(int) git_commit_tree(git_tree **tree_out, git_commit *commit);
   git_commit_tree:                    function (var tree_out: Pgit_tree; commit: Pgit_commit): Integer; stdcall;
   // GIT_EXTERN(const git_oid *) git_commit_tree_oid(git_commit *commit);
   git_commit_tree_oid:                function (commit: Pgit_commit): Pgit_oid; stdcall;
   // GIT_EXTERN(unsigned int) git_commit_parentcount(git_commit *commit);
   git_commit_parentcount:             function (commit: Pgit_commit): UInt; stdcall;
   // GIT_EXTERN(int) git_commit_parent(git_commit **parent, git_commit *commit, unsigned int n);
   git_commit_parent:                  function (var parent: Pgit_commit; commit: Pgit_commit; n: UInt): Integer; stdcall;
   // GIT_EXTERN(const git_oid *) git_commit_parent_oid(git_commit *commit, unsigned int n);
   git_commit_parent_oid:              function (commit: Pgit_commit; n: UInt): Pgit_oid; stdcall;
   // GIT_EXTERN(int) git_commit_create(git_oid *oid, git_repository *repo, const char *update_ref, const git_signature *author, const git_signature *committer, const char *message_encoding, const char *message, const git_tree *tree, int parent_count, const git_commit *parents[]);
   git_commit_create:                  function (oid: Pgit_oid; repo: Pgit_repository; const update_ref: PAnsiChar; const author, committer: Pgit_signature; const message_encoding, message_: PAnsiChar; const tree: Pgit_tree; parent_count: Integer; const parents: PPgit_commit): Integer; stdcall;
   // GIT_EXTERN(int) git_commit_create_v(git_oid *oid, git_repository *repo, const char *update_ref, const git_signature *author, const git_signature *committer, const char *message_encoding, const char *message, const git_tree *tree, int parent_count, ...);
   git_commit_create_v:                function (oid: Pgit_oid; repo: Pgit_repository; const update_ref: PAnsiChar; const author, committer: Pgit_signature; const message_encoding, message_: PAnsiChar; const tree: Pgit_tree; parent_count: Integer): Integer; cdecl varargs; // varargs has to be cdecl

   /// common.h
   ///

   // GIT_EXTERN(void) git_strarray_free(git_strarray *array);
   git_strarray_free:                  procedure (array_: Pgit_strarray); stdcall;
   // GIT_EXTERN(void) git_libgit2_version(int *major, int *minor, int *rev);
   git_libgit2_version:                procedure (out major, minor, rev: Integer); stdcall;

   /// config.h
   ///

   // GIT_EXTERN(int) git_config_find_global(char *global_config_path);
   git_config_find_global:             function (global_config_path: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_config_find_system(char *system_config_path);
   git_config_find_system:             function (system_config_path: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_config_open_global(git_config **out);
   git_config_open_global:             function (var out_: Pgit_config): Integer; stdcall;
   // GIT_EXTERN(int) git_config_file__ondisk(struct git_config_file **out, const char *path);
   git_config_file__ondisk:            function (var out_: Pgit_config_file; const path: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_config_new(git_config **out);
   git_config_new:                     function (var out_: Pgit_config): Integer; stdcall;
   // GIT_EXTERN(int) git_config_add_file(git_config *cfg, git_config_file *file, int priority);
   git_config_add_file:                function (cfg: Pgit_config; file_: Pgit_config_file; priority: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_config_add_file_ondisk(git_config *cfg, const char *path, int priority);
   git_config_add_file_ondisk:         function (cfg: Pgit_config; const path: PAnsiChar; priority: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_config_open_ondisk(git_config **cfg, const char *path);
   git_config_open_ondisk:             function (out cfg: Pgit_config; const path: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(void) git_config_free(git_config *cfg);
   git_config_free:                    procedure (cfg: Pgit_config); stdcall;
   // GIT_EXTERN(int) git_config_get_int32(git_config *cfg, const char *name, int32_t *out);
   git_config_get_int32:               function (cfg: Pgit_config; name: PAnsiChar; out_: PInt32_t): Integer; stdcall;
   // GIT_EXTERN(int) git_config_get_int64(git_config *cfg, const char *name, int64_t *out);
   git_config_get_int64:               function (cfg: Pgit_config; name: PAnsiChar; out_: PInt64_t): Integer; stdcall;
   // GIT_EXTERN(int) git_config_get_bool(git_config *cfg, const char *name, int *out);
   git_config_get_bool:                function (cfg: Pgit_config; const name: PAnsiChar; var out_: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_config_get_string(git_config *cfg, const char *name, const char **out);
   git_config_get_string:              function (cfg: Pgit_config; const name: PAnsiChar; var out_: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_config_set_int32(git_config *cfg, const char *name, int32_t value);
   git_config_set_int32:               function (cfg: Pgit_config; name: PAnsiChar; value: Int32): Integer; stdcall;
   // GIT_EXTERN(int) git_config_set_int64(git_config *cfg, const char *name, int64_t value);
   git_config_set_int64:               function (cfg: Pgit_config; name: PAnsiChar; value: Int64): Integer; stdcall;
   // GIT_EXTERN(int) git_config_set_bool(git_config *cfg, const char *name, int value);
   git_config_set_bool:                function (cfg: Pgit_config; const name: PAnsiChar; value: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_config_set_string(git_config *cfg, const char *name, const char *value);
   git_config_set_string:              function (cfg: Pgit_config; const name: PAnsiChar; const value: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_config_delete(git_config *cfg, const char *name);
   git_config_delete:                  function (cfg: Pgit_config; name: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_config_foreach(git_config *cfg, int (*callback)(const char *var_name, const char *value, void *payload), void *payload);
   git_config_foreach:                 function (cfg: Pgit_config; callback: Pgit_config_foreach_callback; payload: PByte): Integer; stdcall;

   /// errors.h
   ///

   // GIT_EXTERN(const char *) git_lasterror(void);
   git_lasterror:                      function: PAnsiChar; stdcall;
   // GIT_EXTERN(const char *) git_strerror(int num);
   git_strerror:                       function (num: Integer): PAnsiChar; stdcall;
   // GIT_EXTERN(void) git_clearerror(void);
   git_clearerror:                     procedure; stdcall;

   /// index.h
   ///

   // GIT_EXTERN(int) git_index_open(git_index **index, const char *index_path);
   git_index_open:                     function (var index: Pgit_index; const index_path: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(void) git_index_clear(git_index *index);
   git_index_clear:                    procedure (index: Pgit_index); stdcall;
   // GIT_EXTERN(void) git_index_free(git_index *index);
   git_index_free:                     procedure (index: Pgit_index); stdcall;
   // GIT_EXTERN(int) git_index_read(git_index *index);
   git_index_read:                     function (index: Pgit_index): Integer; stdcall;
   // GIT_EXTERN(int) git_index_write(git_index *index);
   git_index_write:                    function (index: Pgit_index): Integer; stdcall;
   // GIT_EXTERN(int) git_index_find(git_index *index, const char *path);
   git_index_find:                     function (index: Pgit_index; const path: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(void) git_index_uniq(git_index *index);
   git_index_uniq:                     procedure (index: Pgit_index); stdcall;
   // GIT_EXTERN(int) git_index_add(git_index *index, const char *path, int stage);
   git_index_add:                      function (index: Pgit_index; const path: PAnsiChar; stage: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_index_add2(git_index *index, const git_index_entry *source_entry);
   git_index_add2:                     function (index: Pgit_index; const source_entry: Pgit_index_entry): Integer; stdcall;
   // GIT_EXTERN(int) git_index_append(git_index *index, const char *path, int stage);
   git_index_append:                   function (index: Pgit_index; const path: PAnsiChar; stage: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_index_append2(git_index *index, const git_index_entry *source_entry);
   git_index_append2:                  function (index: Pgit_index; const source_entry: Pgit_index_entry): Integer; stdcall;
   // GIT_EXTERN(int) git_index_remove(git_index *index, int position);
   git_index_remove:                   function (index: Pgit_index; position: Integer): Integer; stdcall;
   // GIT_EXTERN(git_index_entry *) git_index_get(git_index *index, unsigned int n);
   git_index_get:                      function (index: Pgit_index; n: UInt): Pgit_index_entry; stdcall;
   // GIT_EXTERN(unsigned int) git_index_entrycount(git_index *index);
   git_index_entrycount:               function (index: Pgit_index): UInt; stdcall;
   // GIT_EXTERN(unsigned int) git_index_entrycount_unmerged(git_index *index);
   git_index_entrycount_unmerged:      function (index: Pgit_index): UInt; stdcall;
   // GIT_EXTERN(const git_index_entry_unmerged *) git_index_get_unmerged_bypath(git_index *index, const char *path);
   git_index_get_unmerged_bypath:      function (index: Pgit_index; const path: PAnsiChar): Pgit_index_entry_unmerged; stdcall;
   // GIT_EXTERN(const git_index_entry_unmerged *) git_index_get_unmerged_byindex(git_index *index, unsigned int n);
   git_index_get_unmerged_byindex:     function (index: Pgit_index; n: UInt): Pgit_index_entry_unmerged; stdcall;
   // GIT_EXTERN(int) git_index_entry_stage(const git_index_entry *entry);
   git_index_entry_stage:              function (const entry: Pgit_index_entry): Integer; stdcall;
   // GIT_EXTERN(int) git_index_read_tree(git_index *index, git_tree *tree);
   git_index_read_tree:                function (index: Pgit_index; tree: Pgit_tree): Integer; stdcall;

   /// indexer.h
   ///

   // GIT_EXTERN(int) git_indexer_new(git_indexer **out, const char *packname);
   git_indexer_new:                    function (var out_: Pgit_indexer; packname: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_indexer_run(git_indexer *idx, git_indexer_stats *stats);
   git_indexer_run:                    function (idx: Pgit_indexer; stats: Pgit_indexer_stats): Integer; stdcall;
   // GIT_EXTERN(int) git_indexer_write(git_indexer *idx);
   git_indexer_write:                  function (idx: Pgit_indexer): Integer; stdcall;
   // GIT_EXTERN(const git_oid *) git_indexer_hash(git_indexer *idx);
   git_indexer_hash:                   function (idx: Pgit_indexer): Pgit_oid; stdcall;
   // GIT_EXTERN(void) git_indexer_free(git_indexer *idx);
   git_indexer_free:                   procedure (idx: Pgit_indexer); stdcall;

   /// object.h
   ///

   // GIT_EXTERN(int) git_object_lookup(git_object **object, git_repository *repo, const git_oid *id, git_otype type);
   git_object_lookup:                  function (var object_: Pgit_object; repo: Pgit_repository; const id: Pgit_oid; type_: git_otype): Integer; stdcall;
   // GIT_EXTERN(int) git_object_lookup_prefix(git_object **object_out, git_repository *repo, const git_oid *id, unsigned int len, git_otype type);
   git_object_lookup_prefix:           function (var object_out: Pgit_object; repo: Pgit_repository; const id: Pgit_oid; len: UInt; type_: git_otype): Integer; stdcall;
   // GIT_EXTERN(const git_oid *) git_object_id(const git_object *obj);
   git_object_id:                      function (obj: Pgit_object): Pgit_oid; stdcall;
   // GIT_EXTERN(git_otype) git_object_type(const git_object *obj);
   git_object_type:                    function(obj: Pgit_object): git_otype; stdcall;
   // GIT_EXTERN(git_repository *) git_object_owner(const git_object *obj);
   git_object_owner:                   function (obj: Pgit_object): Pgit_repository; stdcall;
   // GIT_EXTERN(void) git_object_free(git_object *object);
   git_object_free:                    procedure (object_: Pgit_object); stdcall;
   // GIT_EXTERN(const char *) git_object_type2string(git_otype type);
   git_object_type2string:             function (type_: git_otype): PAnsiChar; stdcall;
   // GIT_EXTERN(git_otype) git_object_string2type(const char *str);
   git_object_string2type:             function (const str: PAnsiChar): git_otype; stdcall;
   // GIT_EXTERN(int) git_object_typeisloose(git_otype type);
   git_object_typeisloose:             function (type_: git_otype): Integer; stdcall;
   // GIT_EXTERN(size_t) git_object__size(git_otype type);
   git_object__size:                   function (type_: git_otype): size_t; stdcall;

   /// odb.h
   ///

   // GIT_EXTERN(int) git_odb_new(git_odb **out);
   git_odb_new:                        function (var out_: Pgit_odb): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_open(git_odb **out, const char *objects_dir);
   git_odb_open:                       function (var out_: Pgit_odb; const objects_dir: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_add_backend(git_odb *odb, git_odb_backend *backend, int priority);
   git_odb_add_backend:                function (odb: Pgit_odb; backend: Pgit_odb_backend; priority: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_add_alternate(git_odb *odb, git_odb_backend *backend, int priority);
   git_odb_add_alternate:              function (odb: Pgit_odb; backend: Pgit_odb_backend; priority: Integer): Integer; stdcall;
   // GIT_EXTERN(void) git_odb_free(git_odb *db);
   git_odb_free:                       procedure (db: Pgit_odb); stdcall;
   // GIT_EXTERN(int) git_odb_read(git_odb_object **out, git_odb *db, const git_oid *id);
   git_odb_read:                       function (var out_: Pgit_odb_object; db: Pgit_odb; const id: Pgit_oid): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_read_prefix(git_odb_object **out, git_odb *db, const git_oid *short_id, unsigned int len);
   git_odb_read_prefix:                function (var out_: Pgit_odb_object; db: Pgit_odb; const short_id: Pgit_oid; len: UInt): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_read_header(size_t *len_p, git_otype *type_p, git_odb *db, const git_oid *id);
   git_odb_read_header:                function (var len_p: size_t; var type_p: git_otype; db: Pgit_odb; const id: Pgit_oid): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_exists(git_odb *db, const git_oid *id);
   git_odb_exists:                     function (db: Pgit_odb; const id: Pgit_oid): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_write(git_oid *oid, git_odb *odb, const void *data, size_t len, git_otype type);
   git_odb_write:                      function (oid: Pgit_oid; odb: Pgit_odb; const data: PByte; len: size_t; type_: git_otype): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_open_wstream(git_odb_stream **stream, git_odb *db, size_t size, git_otype type);
   git_odb_open_wstream:               function (var stream: Pgit_odb_stream; db: Pgit_odb; size: size_t; type_: git_otype): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_open_rstream(git_odb_stream **stream, git_odb *db, const git_oid *oid);
   git_odb_open_rstream:               function (var stream: Pgit_odb_stream; db: Pgit_odb; const oid: Pgit_oid): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_hash(git_oid *id, const void *data, size_t len, git_otype type);
   git_odb_hash:                       function (id: Pgit_oid; const data: PByte; len: size_t; type_: git_otype): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_hashfile(git_oid *out, const char *path, git_otype type);
   git_odb_hashfile:                   function (out_: Pgit_oid; path: PAnsiChar; type_: git_otype): Integer; stdcall;
   // GIT_EXTERN(void) git_odb_object_free(git_odb_object *object);
   git_odb_object_free:                procedure (object_: Pgit_odb_object); stdcall;
   // GIT_EXTERN(const git_oid *) git_odb_object_id(git_odb_object *object);
   git_odb_object_id:                  function (object_: Pgit_odb_object): Pgit_oid; stdcall;
   // GIT_EXTERN(const void *) git_odb_object_data(git_odb_object *object);
   git_odb_object_data:                function (object_: Pgit_odb_object): PByte; stdcall;
   // GIT_EXTERN(size_t) git_odb_object_size(git_odb_object *object);
   git_odb_object_size:                function (object_: Pgit_odb_object): size_t; stdcall;
   // GIT_EXTERN(git_otype) git_odb_object_type(git_odb_object *object);
   git_odb_object_type:                function (object_: Pgit_odb_object): git_otype; stdcall;

   /// odb_backend.h
   ///

   // GIT_EXTERN(int) git_odb_backend_pack(git_odb_backend **backend_out, const char *objects_dir);
   git_odb_backend_pack:               function (var backend_out: Pgit_odb_backend; const objects_dir: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_odb_backend_loose(git_odb_backend **backend_out, const char *objects_dir, int compression_level, int do_fsync);
   git_odb_backend_loose:              function (var backend_out: Pgit_odb_backend; const objects_dir: PAnsiChar; compression_leve: Integer; do_fsync: Integer): Integer; stdcall;

   /// oid.h
   ///

   // GIT_EXTERN(int) git_oid_fromstr(git_oid *out, const char *str);
   git_oid_fromstr:                    function (aOut: Pgit_oid; aStr: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_oid_fromstrn(git_oid *out, const char *str, size_t length);
   git_oid_fromstrn:                   function (out_: Pgit_oid; str: PAnsiChar; length: size_t): Integer; stdcall;
   // GIT_EXTERN(void) git_oid_fromraw(git_oid *out, const unsigned char *raw);
   git_oid_fromraw:                    procedure (out_: Pgit_oid; const raw: PByte); stdcall;
   // GIT_EXTERN(void) git_oid_fmt(char *str, const git_oid *oid);
   git_oid_fmt:                        procedure (aStr: PAnsiChar; const oid: Pgit_oid); stdcall;
   // GIT_EXTERN(void) git_oid_pathfmt(char *str, const git_oid *oid);
   git_oid_pathfmt:                    procedure (aStr: PAnsiChar; const oid: Pgit_oid); stdcall;
   // GIT_EXTERN(char *) git_oid_allocfmt(const git_oid *oid);
   git_oid_allocfmt:                   function (const oid: Pgit_oid): PAnsiChar; stdcall;
   // GIT_EXTERN(char *) git_oid_to_string(char *out, size_t n, const git_oid *oid);
   git_oid_to_string:                  function (out_: PAnsiChar; n: size_t; const oid: Pgit_oid): PAnsiChar; stdcall;
   // GIT_EXTERN(void) git_oid_cpy(git_oid *out, const git_oid *src);
   git_oid_cpy:                        procedure (out_: Pgit_oid; const src: Pgit_oid); stdcall;
   // GIT_EXTERN(int) git_oid_cmp(const git_oid *a, const git_oid *b);
   git_oid_cmp:                        function (const a, b: Pgit_oid): Integer; stdcall;
   // GIT_EXTERN(int) git_oid_ncmp(const git_oid *a, const git_oid *b, unsigned int len);
   git_oid_ncmp:                       function (const a, b: Pgit_oid; len: UInt): Integer; stdcall;
   // GIT_EXTERN(int) git_oid_streq(const git_oid *a, const char *str);
   git_oid_streq:                      function (a: Pgit_oid; str: PAnsiChar): Integer; stdcall;

   /// reflog.h
   ///

   // GIT_EXTERN(int) git_reflog_read(git_reflog **reflog, git_reference *ref);
   git_reflog_read:                    function (var reflog: Pgit_reflog; ref: Pgit_reference): Integer; stdcall;
   // GIT_EXTERN(int) git_reflog_write(git_reference *ref, const git_oid *oid_old, const git_signature *committer, const char *msg);
   git_reflog_write:                   function (ref: Pgit_reference; oid_old: Pgit_oid; committer: Pgit_signature; msg: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_reflog_rename(git_reference *ref, const char *new_name);
   git_reflog_rename:                  function (ref: Pgit_reference; new_name: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_reflog_delete(git_reference *ref);
   git_reflog_delete:                  function (ref: Pgit_reference): Integer; stdcall;
   // GIT_EXTERN(unsigned int) git_reflog_entrycount(git_reflog *reflog);
   git_reflog_entrycount:              function (reflog: Pgit_reflog): UInt; stdcall;
   // GIT_EXTERN(const git_reflog_entry *) git_reflog_entry_byindex(git_reflog *reflog, unsigned int idx);
   git_reflog_entry_byindex:           function (reflog: Pgit_reflog; idx: UInt): Pgit_reflog_entry; stdcall;
   // GIT_EXTERN(const git_oid *) git_reflog_entry_oidold(const git_reflog_entry *entry);
   git_reflog_entry_oidold:            function (entry: Pgit_reflog_entry): Pgit_oid; stdcall;
   // GIT_EXTERN(const git_oid *) git_reflog_entry_oidnew(const git_reflog_entry *entry);
   git_reflog_entry_oidnew:            function (entry: Pgit_reflog_entry): Pgit_oid; stdcall;
   // GIT_EXTERN(git_signature *) git_reflog_entry_committer(const git_reflog_entry *entry);
   git_reflog_entry_committer:         function (entry: Pgit_reflog_entry): Pgit_signature; stdcall;
   // GIT_EXTERN(char *) git_reflog_entry_msg(const git_reflog_entry *entry);
   git_reflog_entry_msg:               function (entry: Pgit_reflog_entry): PAnsiChar; stdcall;
   // GIT_EXTERN(void) git_reflog_free(git_reflog *reflog);
   git_reflog_free:                    procedure (reflog: Pgit_reflog); stdcall;

   /// refs.h
   ///

   // GIT_EXTERN(int) git_reference_lookup(git_reference **reference_out, git_repository *repo, const char *name);
   git_reference_lookup:               function (var reference_out: Pgit_reference; repo: Pgit_repository; const name: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_reference_create_symbolic(git_reference **ref_out, git_repository *repo, const char *name, const char *target, int force);
   git_reference_create_symbolic:      function (var ref_out: Pgit_reference; repo: Pgit_repository; const name: PAnsiChar; const target: PAnsiChar; force: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_reference_create_oid(git_reference **ref_out, git_repository *repo, const char *name, const git_oid *id, int force);
   git_reference_create_oid:           function (var ref_out: Pgit_reference; repo: Pgit_repository; const name: PAnsiChar; const id: Pgit_oid; force: Integer): Integer; stdcall;
   // GIT_EXTERN(const git_oid *) git_reference_oid(git_reference *ref);
   git_reference_oid:                  function (ref: Pgit_reference): Pgit_oid; stdcall;
   // GIT_EXTERN(const char *) git_reference_target(git_reference *ref);
   git_reference_target:               function (ref: Pgit_reference): PAnsiChar; stdcall;
   // GIT_EXTERN(git_rtype) git_reference_type(git_reference *ref);
   git_reference_type:                 function (ref: Pgit_reference): git_rtype; stdcall;
   // GIT_EXTERN(const char *) git_reference_name(git_reference *ref);
   git_reference_name:                 function (ref: Pgit_reference): PAnsiChar; stdcall;
   // GIT_EXTERN(int) git_reference_resolve(git_reference **resolved_ref, git_reference *ref);
   git_reference_resolve:              function (var resolved_ref: Pgit_reference; ref: Pgit_reference): Integer; stdcall;
   // GIT_EXTERN(git_repository *) git_reference_owner(git_reference *ref);
   git_reference_owner:                function (ref: Pgit_reference): Pgit_repository; stdcall;
   // GIT_EXTERN(int) git_reference_set_target(git_reference *ref, const char *target);
   git_reference_set_target:           function (ref: Pgit_reference; const target: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_reference_set_oid(git_reference *ref, const git_oid *id);
   git_reference_set_oid:              function (ref: Pgit_reference; const id: Pgit_oid): Integer; stdcall;
   // GIT_EXTERN(int) git_reference_rename(git_reference *ref, const char *new_name, int force);
   git_reference_rename:               function (ref: Pgit_reference; const new_name: PAnsiChar; force: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_reference_delete(git_reference *ref);
   git_reference_delete:               function (ref: Pgit_reference): Integer; stdcall;
   // GIT_EXTERN(int) git_reference_packall(git_repository *repo);
   git_reference_packall:              function (repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(int) git_reference_listall(git_strarray *array, git_repository *repo, unsigned int list_flags);
   git_reference_listall:              function (array_: Pgit_strarray; repo: Pgit_repository; list_flags: UInt): Integer; stdcall;
   // GIT_EXTERN(int) git_reference_foreach(git_repository *repo, unsigned int list_flags, int (*callback)(const char *, void *), void *payload);
   git_reference_foreach:              function (repo: Pgit_repository; list_flags: UInt; callback: Pgit_reference_foreach_callback; payload: PByte): Integer; stdcall;
   // GIT_EXTERN(int) git_reference_is_packed(git_reference *ref);
   git_reference_is_packed:            function (ref: Pgit_reference): Integer; stdcall;
   // GIT_EXTERN(int) git_reference_reload(git_reference *ref);
   git_reference_reload:               function (ref: Pgit_reference): Integer; stdcall;
   // GIT_EXTERN(void) git_reference_free(git_reference *ref);
   git_reference_free:                 procedure (ref: Pgit_reference); stdcall;

   /// remote.h
   ///

   // GIT_EXTERN(int) git_remote_new(git_remote **out, git_repository *repo, const char *url, const char *name);
   git_remote_new:                     function (var out_: Pgit_remote; repo: Pgit_repository; url, name: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_remote_load(git_remote **out, git_repository *repo, const char *name);
   git_remote_load:                    function (var out_: Pgit_remote; repo: Pgit_repository; name: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(const char *) git_remote_name(git_remote *remote);
   git_remote_name:                    function (remote: Pgit_remote): PAnsiChar; stdcall;
   // GIT_EXTERN(const char *) git_remote_url(git_remote *remote);
   git_remote_url:                     function (remote: Pgit_remote): PAnsiChar; stdcall;
   // GIT_EXTERN(const git_refspec *) git_remote_fetchspec(git_remote *remote);
   git_remote_fetchspec:               function (remote: Pgit_remote): Pgit_refspec; stdcall;
   // GIT_EXTERN(const git_refspec *) git_remote_pushspec(git_remote *remote);
   git_remote_pushspec:                function (remote: Pgit_remote): Pgit_refspec; stdcall;
   // GIT_EXTERN(int) git_remote_connect(git_remote *remote, int direction);
   git_remote_connect:                 function (remote: Pgit_remote; direction: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_remote_ls(git_remote *remote, git_headlist_cb list_cb, void *payload);
   git_remote_ls:                      function (remote: Pgit_remote; list_cb: git_headlist_cb; payload: PByte): Integer; stdcall;
   // GIT_EXTERN(int) git_remote_download(char **filename, git_remote *remote);
   git_remote_download:                function (var filename: PAnsiChar; remote: Pgit_remote): Integer; stdcall;
   // GIT_EXTERN(int) git_remote_connected(git_remote *remote);
   git_remote_connected:               function (remote: Pgit_remote): Integer; stdcall;
   // GIT_EXTERN(void) git_remote_disconnect(git_remote *remote);
   git_remote_disconnect:              procedure (remote: Pgit_remote); stdcall;
   // GIT_EXTERN(void) git_remote_free(git_remote *remote);
   git_remote_free:                    procedure (remote: Pgit_remote); stdcall;
   // GIT_EXTERN(int) git_remote_update_tips(git_remote *remote);
   git_remote_update_tips:             function (remote: Pgit_remote): Integer; stdcall;
   // GIT_EXTERN(int) git_remote_valid_url(const char *url);
   git_remote_valid_url:               function (url: PAnsiChar): Integer; stdcall;

   /// repository.h
   ///

   // GIT_EXTERN(int) git_repository_open(git_repository **repository, const char *path);
   git_repository_open:                function (var repo_out: Pgit_repository; const path: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_repository_discover(char *repository_path, size_t size, const char *start_path, int across_fs, const char *ceiling_dirs);
   git_repository_discover:            function (repository_path: PAnsiChar; size: size_t; const start_path: PAnsiChar; across_fs: Integer; const ceiling_dirs: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(void) git_repository_free(git_repository *repo);
   git_repository_free:                procedure (repo: Pgit_repository); stdcall;
   // GIT_EXTERN(int) git_repository_init(git_repository **repo_out, const char *path, unsigned is_bare);
   git_repository_init:                function (var repo_out: Pgit_repository; const path: PAnsiChar; is_bare: UInt): Integer; stdcall;
   // GIT_EXTERN(int) git_repository_head(git_reference **head_out, git_repository *repo);
   git_repository_head:                function (var head_out: Pgit_reference; repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(int) git_repository_head_detached(git_repository *repo);
   git_repository_head_detached:       function (repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(int) git_repository_head_orphan(git_repository *repo);
   git_repository_head_orphan:         function (repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(int) git_repository_is_empty(git_repository *repo);
   git_repository_is_empty:            function (repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(const char *) git_repository_path(git_repository *repo);
   git_repository_path:                function (repo: Pgit_repository): PAnsiChar; stdcall;
   // GIT_EXTERN(const char *) git_repository_workdir(git_repository *repo);
   git_repository_workdir:             function (repo: Pgit_repository): PAnsiChar; stdcall;
   // GIT_EXTERN(int) git_repository_set_workdir(git_repository *repo, const char *workdir);
   git_repository_set_workdir:         function (repo: Pgit_repository; workdir: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_repository_is_bare(git_repository *repo);
   git_repository_is_bare:             function (repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(int) git_repository_config(git_config **out, git_repository *repo);
   git_repository_config:              function (var out_: Pgit_config; repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(void) git_repository_set_config(git_repository *repo, git_config *config);
   git_repository_set_config:          procedure (repo: Pgit_repository; config: Pgit_config); stdcall;
   // GIT_EXTERN(int) git_repository_odb(git_odb **out, git_repository *repo);
   git_repository_odb:                 function (var out_: Pgit_odb; repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(void) git_repository_set_odb(git_repository *repo, git_odb *odb);
   git_repository_set_odb:             procedure (repo: Pgit_repository; odb: Pgit_odb); stdcall;
   // GIT_EXTERN(int) git_repository_index(git_index **index, git_repository *repo);
   git_repository_index:               function (var index: Pgit_index; repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(void) git_repository_set_index(git_repository *repo, git_index *index);
   git_repository_set_index:           procedure (repo: Pgit_repository; index: Pgit_index); stdcall;

   /// revwalk.h
   ///

   // GIT_EXTERN(int) git_revwalk_new(git_revwalk **walker, git_repository *repo);
   git_revwalk_new:                    function (var walker: Pgit_revwalk; repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(void) git_revwalk_reset(git_revwalk *walker);
   git_revwalk_reset:                  procedure (walker: Pgit_revwalk); stdcall;
   // GIT_EXTERN(int) git_revwalk_push(git_revwalk *walk, const git_oid *oid);
   git_revwalk_push:                   function (walk: Pgit_revwalk; const oid: Pgit_oid): Integer; stdcall;
   // GIT_EXTERN(int) git_revwalk_hide(git_revwalk *walk, const git_oid *oid);
   git_revwalk_hide:                   function (walk: Pgit_revwalk; const oid: Pgit_oid): Integer; stdcall;
   // GIT_EXTERN(int) git_revwalk_next(git_oid *oid, git_revwalk *walk);
   git_revwalk_next:                   function (oid: Pgit_oid; walk: Pgit_revwalk): Integer; stdcall;
   // GIT_EXTERN(void) git_revwalk_sorting(git_revwalk *walk, unsigned int sort_mode);
   git_revwalk_sorting:                procedure (walk: Pgit_revwalk; sort_mode: UInt); stdcall;
   // GIT_EXTERN(void) git_revwalk_free(git_revwalk *walk);
   git_revwalk_free:                   procedure (walk: Pgit_revwalk); stdcall;
   // GIT_EXTERN(git_repository *) git_revwalk_repository(git_revwalk *walk);
   git_revwalk_repository:             function (walk: Pgit_revwalk): Pgit_repository; stdcall;

   /// signature.h
   ///

   // GIT_EXTERN(int) git_signature_new(git_signature **sig_out, const char *name, const char *email, git_time_t time, int offset);
   git_signature_new:                  function (var sig_out: Pgit_signature; const name, email: PAnsiChar; time: git_time_t; offset: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_signature_now(git_signature **sig_out, const char *name, const char *email);
   git_signature_now:                  function (var sig_out: Pgit_signature; const name, email: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(git_signature *) git_signature_dup(const git_signature *sig);
   git_signature_dup:                  function (const sig: Pgit_signature): Pgit_signature; stdcall;
   // GIT_EXTERN(void) git_signature_free(git_signature *sig);
   git_signature_free:                 procedure (sig: Pgit_signature); stdcall;

   /// status.h
   ///

   // GIT_EXTERN(int) git_status_foreach(git_repository *repo, int (*callback)(const char *, unsigned int, void *), void *payload);
   git_status_foreach:                 function (repo: Pgit_repository; callback: Pgit_status_foreach_callback; payload: PByte): Integer; stdcall;
   // GIT_EXTERN(int) git_status_file(unsigned int *status_flags, git_repository *repo, const char *path);
   git_status_file:                    function (status_flags: PUint; repo: Pgit_repository; path: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_status_should_ignore(git_repository *repo, const char *path, int *ignored);
   git_status_should_ignore:           function (repo: Pgit_repository; path: PAnsiChar; ignored: PInt): Integer; stdcall;

   /// tag.h
   ///

   // GIT_EXTERN(const git_oid *) git_tag_id(git_tag *tag);
   git_tag_id:                         function (tag: Pgit_tag): Pgit_oid; stdcall;
   // GIT_EXTERN(int) git_tag_target(git_object **target, git_tag *tag);
   git_tag_target:                     function (var target: Pgit_object; tag: Pgit_tag): Integer; stdcall;
   // GIT_EXTERN(const git_oid *) git_tag_target_oid(git_tag *tag);
   git_tag_target_oid:                 function (tag: Pgit_tag): Pgit_oid; stdcall;
   // GIT_EXTERN(git_otype) git_tag_type(git_tag *tag);
   git_tag_type:                       function (tag: Pgit_tag): git_otype; stdcall;
   // GIT_EXTERN(const char *) git_tag_name(git_tag *tag);
   git_tag_name:                       function (tag: Pgit_tag): PAnsiChar; stdcall;
   // GIT_EXTERN(const git_signature *) git_tag_tagger(git_tag *tag);
   git_tag_tagger:                     function (tag: Pgit_tag): Pgit_signature; stdcall;
   // GIT_EXTERN(const char *) git_tag_message(git_tag *tag);
   git_tag_message:                    function (tag: Pgit_tag): PAnsiChar; stdcall;
   // GIT_EXTERN(int) git_tag_create(git_oid *oid, git_repository *repo, const char *tag_name, const git_object *target, const git_signature *tagger, const char *message, int force);
   git_tag_create:                     function (oid: Pgit_oid; repo: Pgit_repository; const tag_name: PAnsiChar; const target: Pgit_object; const tagger: Pgit_signature; const message_: PAnsiChar; force: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_tag_create_frombuffer(git_oid *oid, git_repository *repo, const char *buffer, int force);
   git_tag_create_frombuffer:          function (oid: Pgit_oid; repo: Pgit_repository; const buffer: PAnsiChar; force: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_tag_create_lightweight(git_oid *oid, git_repository *repo, const char *tag_name, const git_object *target, int force);
   git_tag_create_lightweight:         function (oid: Pgit_oid; repo: Pgit_repository; tag_name: PAnsiChar; target: Pgit_object; force: Integer): Integer; stdcall;
   // GIT_EXTERN(int) git_tag_delete(git_repository *repo, const char *tag_name);
   git_tag_delete:                     function (repo: Pgit_repository; const tag_name: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_tag_list(git_strarray *tag_names, git_repository *repo);
   git_tag_list:                       function (tag_names: Pgit_strarray; repo: Pgit_repository): Integer; stdcall;
   // GIT_EXTERN(int) git_tag_list_match(git_strarray *tag_names, const char *pattern, git_repository *repo);
   git_tag_list_match:                 function (tag_names: Pgit_strarray; pattern: PAnsiChar; repo: Pgit_repository): Integer; stdcall;

   /// threads.h
   ///

   // GIT_EXTERN(void) git_threads_init(void);
   git_threads_init:                   procedure; stdcall;
   // GIT_EXTERN(void) git_threads_shutdown(void);
   git_threads_shutdown:               procedure; stdcall;

   /// tree.h
   ///

   // GIT_EXTERN(const git_oid *) git_tree_id(git_tree *tree);
   git_tree_id:                        function (tree: Pgit_tree): Pgit_oid; stdcall;
   // GIT_EXTERN(unsigned int) git_tree_entrycount(git_tree *tree);
   git_tree_entrycount:                function (tree: Pgit_tree): UInt; stdcall;
   // GIT_EXTERN(const git_tree_entry *) git_tree_entry_byname(git_tree *tree, const char *filename);
   git_tree_entry_byname:              function (tree: Pgit_tree; const filename: PAnsiChar): Pgit_tree_entry; stdcall;
   // GIT_EXTERN(const git_tree_entry *) git_tree_entry_byindex(git_tree *tree, unsigned int idx);
   git_tree_entry_byindex:             function (tree: Pgit_tree; idx: UInt): Pgit_tree_entry; stdcall;
   // GIT_EXTERN(unsigned int) git_tree_entry_attributes(const git_tree_entry *entry);
   git_tree_entry_attributes:          function (const entry: Pgit_tree_entry): UInt; stdcall;
   // GIT_EXTERN(const char *) git_tree_entry_name(const git_tree_entry *entry);
   git_tree_entry_name:                function (const entry: Pgit_tree_entry): PAnsiChar; stdcall;
   // GIT_EXTERN(const git_oid *) git_tree_entry_id(const git_tree_entry *entry);
   git_tree_entry_id:                  function (const entry: Pgit_tree_entry): Pgit_oid; stdcall;
   // GIT_EXTERN(git_otype) git_tree_entry_type(const git_tree_entry *entry);
   git_tree_entry_type:                function (const entry: Pgit_tree_entry): git_otype; stdcall;
   // GIT_EXTERN(int) git_tree_entry_2object(git_object **object_out, git_repository *repo, const git_tree_entry *entry);
   git_tree_entry_2object:             function (var object_out: Pgit_object; repo: Pgit_repository; const entry: Pgit_tree_entry): Integer; stdcall;
   // GIT_EXTERN(int) git_tree_create_fromindex(git_oid *oid, git_index *index);
   git_tree_create_fromindex:          function (oid: Pgit_oid; index: Pgit_index): Integer; stdcall;

   // GIT_EXTERN(int) git_treebuilder_create(git_treebuilder **builder_p, const git_tree *source);
   git_treebuilder_create:             function (var builder_p: Pgit_treebuilder; const source: Pgit_tree): Integer; stdcall;
   // GIT_EXTERN(void) git_treebuilder_clear(git_treebuilder *bld);
   git_treebuilder_clear:              procedure (bld: Pgit_treebuilder); stdcall;
   // GIT_EXTERN(void) git_treebuilder_free(git_treebuilder *bld);
   git_treebuilder_free:               procedure (bld: Pgit_treebuilder); stdcall;
   // GIT_EXTERN(const git_tree_entry *) git_treebuilder_get(git_treebuilder *bld, const char *filename);
   git_treebuilder_get:                function (bld: Pgit_treebuilder; const filename: PAnsiChar): Pgit_tree_entry; stdcall;
   // GIT_EXTERN(int) git_treebuilder_insert(git_tree_entry **entry_out, git_treebuilder *bld, const char *filename, const git_oid *id, unsigned int attributes);
   git_treebuilder_insert:             function (var entry_out: Pgit_tree_entry; bld: Pgit_treebuilder; const filename: PAnsiChar; const id: Pgit_oid; attributes: UInt): Integer; stdcall;
   // GIT_EXTERN(int) git_treebuilder_remove(git_treebuilder *bld, const char *filename);
   git_treebuilder_remove:             function (bld: Pgit_treebuilder; const filename: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(void) git_treebuilder_filter(git_treebuilder *bld, int (*filter)(const git_tree_entry *, void *), void *payload);
   git_treebuilder_filter:             procedure (bld: Pgit_treebuilder; filter: Pgit_treebuilder_filter_filter; payload: PByte); stdcall;
   // GIT_EXTERN(int) git_treebuilder_write(git_oid *oid, git_repository *repo, git_treebuilder *bld);
   git_treebuilder_write:              function (oid:Pgit_oid; repo: Pgit_repository; bld: Pgit_treebuilder): Integer; stdcall;
   // GIT_EXTERN(int) git_tree_get_subtree(git_tree **subtree, git_tree *root, const char *subtree_path);
   git_tree_get_subtree:               function (var subtree: Pgit_tree; root: Pgit_tree; subtree_path: PAnsiChar): Integer; stdcall;
   // GIT_EXTERN(int) git_tree_walk(git_tree *tree, git_treewalk_cb callback, int mode, void *payload);
   git_tree_walk:                      function (tree: Pgit_tree; callback: Pgit_treewalk_cb; mode: Integer; payload: PByte): Integer;

   /// windows.h
   ///

   // GIT_EXTERN(void) gitwin_set_codepage(unsigned int codepage);
   gitwin_set_codepage:                procedure (codepage: UInt); stdcall;
   // GIT_EXTERN(unsigned int) gitwin_get_codepage(void);
   gitwin_get_codepage:                function (): UInt; stdcall;
   // GIT_EXTERN(void) gitwin_set_utf8(void);
   gitwin_set_utf8:                    procedure (); stdcall;

// GIT_EXTERNs later inlined
function git_blob_lookup(var blob: Pgit_blob; repo: Pgit_repository; const id: Pgit_oid): Integer; stdcall;
function git_commit_lookup(var commit: Pgit_commit; repo: Pgit_repository; const id: Pgit_oid): Integer; stdcall;
function git_tag_lookup(var tag: Pgit_tag; repo: Pgit_repository; const id: Pgit_oid): Integer; stdcall;
function git_tree_lookup(var tree: Pgit_tree; repo: Pgit_repository; const id: Pgit_oid): Integer; stdcall;
procedure git_commit_free(commit: Pgit_commit);
procedure git_tag_free(tag: Pgit_tag);
procedure git_tree_free(tree: Pgit_tree);

// Helpers
function time_t__to__TDateTime(t: time_t; const aAdjustMinutes: Integer = 0): TDateTime;
function git_commit_DateTime(commit: Pgit_commit): TDateTime;

implementation

var
  libgit2: THandle;

function git_blob_lookup(var blob: Pgit_blob; repo: Pgit_repository; const id: Pgit_oid): Integer; stdcall;
begin
   // return git_object_lookup((git_object **)blob, repo, id, GIT_OBJ_BLOB);
   Result := git_object_lookup(Pgit_object(blob), repo, id, GIT_OBJ_BLOB);
end;

function git_commit_lookup(var commit: Pgit_commit; repo: Pgit_repository; const id: Pgit_oid): Integer; stdcall;
begin
   // return git_object_lookup((git_object **)commit, repo, id, GIT_OBJ_COMMIT);
   Result := git_object_lookup(commit, repo, id, GIT_OBJ_COMMIT);
end;

function git_tag_lookup(var tag: Pgit_tag; repo: Pgit_repository; const id: Pgit_oid): Integer; stdcall;
begin
   // return git_object_lookup((git_object **)tag, repo, id, GIT_OBJ_TAG);
   Result := git_object_lookup(Pgit_object(tag), repo, id, GIT_OBJ_TAG);
end;

function git_tree_lookup(var tree: Pgit_tree; repo: Pgit_repository; const id: Pgit_oid): Integer; stdcall;
begin
   // return git_object_lookup((git_object **)tree, repo, id, GIT_OBJ_TREE);
   Result := git_object_lookup(Pgit_object(tree), repo, id, GIT_OBJ_TREE);
end;

procedure git_commit_free(commit: Pgit_commit);
begin
   git_object_free(commit);
end;

procedure git_tag_free(tag: Pgit_tag);
begin
   git_object_free(tag);
end;

procedure git_tree_free(tree: Pgit_tree);
begin
   git_object_free(tree);
end;

function time_t__to__TDateTime(t: time_t; const aAdjustMinutes: Integer = 0): TDateTime;
const
   UnixStartDate: TDateTime = 25569.0; // 01/01/1970
begin
   Result := (t / SecsPerDay) + UnixStartDate;                          //   Result := DateUtils.IncSecond(EncodeDate(1970,1,1), t);
   if aAdjustMinutes <> 0 then
      Result := ((Result * MinsPerDay) + aAdjustMinutes) / MinsPerDay;  //      Result := DateUtils.IncMinute(Result, aAdjustMinutes);
end;

function git_commit_DateTime(commit: Pgit_commit): TDateTime;
var
   t: time_t;
   time_offset: Integer;
begin
   t := git_commit_time(commit);
   time_offset := git_commit_time_offset(commit);

   Result := time_t__to__TDateTime(t, time_offset);
end;

procedure BindFuncs(aBind: Boolean);
   function Bind(const aName, aModifier: AnsiString; aAllowNotFound: Boolean = false): Pointer;
   var
      exported_name: AnsiString;
   begin
      if not aBind then
         Result := nil
      else
      begin
         if aModifier = '' then
            exported_name := aName
         else
            exported_name := '_' + aName + '@' + aModifier;

         Result := GetProcAddress(libgit2, PAnsiChar(exported_name));
         {$IFDEF DEBUG}
         if (Result = nil) and (not aAllowNotFound) then
            raise Exception.CreateFmt('Export [%s] not found (as %s)', [aName, exported_name]);
         {$ENDIF}
      end;
   end;
const
   OPTIONAL = true;
begin
   git_attr__true                   := Bind('git_attr__true', '');
   git_attr__false                  := Bind('git_attr__false', '');

   git_attr_get                     := Bind('git_attr_get', '16');
   git_attr_get_many                := Bind('git_attr_get_many', '20');
   git_attr_foreach                 := Bind('git_attr_foreach', '16');
   git_attr_cache_flush             := Bind('git_attr_cache_flush', '4');
   git_attr_add_macro               := Bind('git_attr_add_macro', '12');

   git_blob_rawcontent              := Bind('git_blob_rawcontent', '4');
   git_blob_rawsize                 := Bind('git_blob_rawsize', '4');
   git_blob_create_fromfile         := Bind('git_blob_create_fromfile', '12');
   git_blob_create_frombuffer       := Bind('git_blob_create_frombuffer', '16');

   git_commit_id                    := Bind('git_commit_id', '4');
   git_commit_message_encoding      := Bind('git_commit_message_encoding', '4');
   git_commit_message               := Bind('git_commit_message', '4');
   git_commit_time                  := Bind('git_commit_time', '4');
   git_commit_time_offset           := Bind('git_commit_time_offset', '4');
   git_commit_committer             := Bind('git_commit_committer', '4');
   git_commit_author                := Bind('git_commit_author', '4');
   git_commit_tree                  := Bind('git_commit_tree', '8');
   git_commit_tree_oid              := Bind('git_commit_tree_oid', '4');
   git_commit_parentcount           := Bind('git_commit_parentcount', '4');
   git_commit_parent                := Bind('git_commit_parent', '12');
   git_commit_parent_oid            := Bind('git_commit_parent_oid', '8');
   git_commit_create                := Bind('git_commit_create', '40');
   git_commit_create_v              := Bind('git_commit_create_v', '');

   git_strarray_free                := Bind('git_strarray_free', '4');
   git_libgit2_version              := Bind('git_libgit2_version', '12');

   git_config_find_global           := Bind('git_config_find_global', '4');
   git_config_find_system           := Bind('git_config_find_system', '4');
   git_config_open_global           := Bind('git_config_open_global', '4');
   git_config_file__ondisk          := Bind('git_config_file__ondisk', '8');
   git_config_new                   := Bind('git_config_new', '4');
   git_config_add_file              := Bind('git_config_add_file', '12');
   git_config_add_file_ondisk       := Bind('git_config_add_file_ondisk', '12');
   git_config_open_ondisk           := Bind('git_config_open_ondisk', '8');
   git_config_free                  := Bind('git_config_free', '4');
   git_config_get_int32             := Bind('git_config_get_int32', '12');
   git_config_get_int64             := Bind('git_config_get_int64', '12');
   git_config_get_bool              := Bind('git_config_get_bool', '12');
   git_config_get_string            := Bind('git_config_get_string', '12');
   git_config_set_int32             := Bind('git_config_set_int32', '12');
   git_config_set_int64             := Bind('git_config_set_int64', '16');
   git_config_set_bool              := Bind('git_config_set_bool', '12');
   git_config_set_string            := Bind('git_config_set_string', '12');
   git_config_delete                := Bind('git_config_delete', '8');
   git_config_foreach               := Bind('git_config_foreach', '12');

   git_lasterror                    := Bind('git_lasterror', '0');
   git_strerror                     := Bind('git_strerror', '4');
   git_clearerror                   := Bind('git_clearerror', '0');

   git_index_open                   := Bind('git_index_open', '8');
   git_index_clear                  := Bind('git_index_clear', '4');
   git_index_free                   := Bind('git_index_free', '4');
   git_index_read                   := Bind('git_index_read', '4');
   git_index_write                  := Bind('git_index_write', '4');
   git_index_find                   := Bind('git_index_find', '8');
   git_index_uniq                   := Bind('git_index_uniq', '4');
   git_index_add                    := Bind('git_index_add', '12');
   git_index_add2                   := Bind('git_index_add2', '8');
   git_index_append                 := Bind('git_index_append', '12');
   git_index_append2                := Bind('git_index_append2', '8');
   git_index_remove                 := Bind('git_index_remove', '8');
   git_index_get                    := Bind('git_index_get', '8');
   git_index_entrycount             := Bind('git_index_entrycount', '4');
   git_index_entrycount_unmerged    := Bind('git_index_entrycount_unmerged', '4');
   git_index_get_unmerged_bypath    := Bind('git_index_get_unmerged_bypath', '8');
   git_index_get_unmerged_byindex   := Bind('git_index_get_unmerged_byindex', '8');
   git_index_entry_stage            := Bind('git_index_entry_stage', '4');
   git_index_read_tree              := Bind('git_index_read_tree', '8');

   git_indexer_new                  := Bind('git_indexer_new', '8');
   git_indexer_run                  := Bind('git_indexer_run', '8');
   git_indexer_write                := Bind('git_indexer_write', '4');
   git_indexer_hash                 := Bind('git_indexer_hash', '4');
   git_indexer_free                 := Bind('git_indexer_free', '4');

   git_object_lookup                := Bind('git_object_lookup', '16');
   git_object_lookup_prefix         := Bind('git_object_lookup_prefix', '20');
   git_object_id                    := Bind('git_object_id', '4');
   git_object_type                  := Bind('git_object_type', '4');
   git_object_owner                 := Bind('git_object_owner', '4');
   git_object_free                  := Bind('git_object_free', '4');
   git_object_type2string           := Bind('git_object_type2string', '4');
   git_object_string2type           := Bind('git_object_string2type', '4');
   git_object_typeisloose           := Bind('git_object_typeisloose', '4');
   git_object__size                 := Bind('git_object__size', '4');

   git_odb_new                      := Bind('git_odb_new', '4');
   git_odb_open                     := Bind('git_odb_open', '8');
   git_odb_add_backend              := Bind('git_odb_add_backend', '12');
   git_odb_add_alternate            := Bind('git_odb_add_alternate', '12');
   git_odb_free                     := Bind('git_odb_free', '4');
   git_odb_read                     := Bind('git_odb_read', '12');
   git_odb_read_prefix              := Bind('git_odb_read_prefix', '16');
   git_odb_read_header              := Bind('git_odb_read_header', '16');
   git_odb_exists                   := Bind('git_odb_exists', '8');
   git_odb_write                    := Bind('git_odb_write', '20');
   git_odb_open_wstream             := Bind('git_odb_open_wstream', '16');
   git_odb_open_rstream             := Bind('git_odb_open_rstream', '12');
   git_odb_hash                     := Bind('git_odb_hash', '16');
   git_odb_hashfile                 := Bind('git_odb_hashfile', '12');
   git_odb_object_free              := Bind('git_odb_object_free', '4');
   git_odb_object_id                := Bind('git_odb_object_id', '4');
   git_odb_object_data              := Bind('git_odb_object_data', '4');
   git_odb_object_size              := Bind('git_odb_object_size', '4');
   git_odb_object_type              := Bind('git_odb_object_type', '4');

   git_odb_backend_pack             := Bind('git_odb_backend_pack', '8');
   git_odb_backend_loose            := Bind('git_odb_backend_loose', '16');

   git_oid_fromstr                  := Bind('git_oid_fromstr', '8');
   git_oid_fromstrn                 := Bind('git_oid_fromstrn', '12');
   git_oid_fromraw                  := Bind('git_oid_fromraw', '8');
   git_oid_fmt                      := Bind('git_oid_fmt', '8');
   git_oid_pathfmt                  := Bind('git_oid_pathfmt', '8');
   git_oid_allocfmt                 := Bind('git_oid_allocfmt', '4');
   git_oid_to_string                := Bind('git_oid_to_string', '12');
   git_oid_cpy                      := Bind('git_oid_cpy', '8');
   git_oid_cmp                      := Bind('git_oid_cmp', '8');
   git_oid_ncmp                     := Bind('git_oid_ncmp', '12');
   git_oid_streq                    := Bind('git_oid_streq', '8');

   git_reflog_read                  := Bind('git_reflog_read', '8');
   git_reflog_write                 := Bind('git_reflog_write', '16');
   git_reflog_rename                := Bind('git_reflog_rename', '8');
   git_reflog_delete                := Bind('git_reflog_delete', '4');
   git_reflog_entrycount            := Bind('git_reflog_entrycount', '4');
   git_reflog_entry_byindex         := Bind('git_reflog_entry_byindex', '8');
   git_reflog_entry_oidold          := Bind('git_reflog_entry_oidold', '4');
   git_reflog_entry_oidnew          := Bind('git_reflog_entry_oidnew', '4');
   git_reflog_entry_committer       := Bind('git_reflog_entry_committer', '4');
   git_reflog_entry_msg             := Bind('git_reflog_entry_msg', '4');
   git_reflog_free                  := Bind('git_reflog_free', '4');

   git_reference_lookup             := Bind('git_reference_lookup', '12');
   git_reference_create_symbolic    := Bind('git_reference_create_symbolic', '20');
   git_reference_create_oid         := Bind('git_reference_create_oid', '20');
   git_reference_oid                := Bind('git_reference_oid', '4');
   git_reference_target             := Bind('git_reference_target', '4');
   git_reference_type               := Bind('git_reference_type', '4');
   git_reference_name               := Bind('git_reference_name', '4');
   git_reference_resolve            := Bind('git_reference_resolve', '8');
   git_reference_owner              := Bind('git_reference_owner', '4');
   git_reference_set_target         := Bind('git_reference_set_target', '8');
   git_reference_set_oid            := Bind('git_reference_set_oid', '8');
   git_reference_rename             := Bind('git_reference_rename', '12');
   git_reference_delete             := Bind('git_reference_delete', '4');
   git_reference_packall            := Bind('git_reference_packall', '4');
   git_reference_listall            := Bind('git_reference_listall', '12');
   git_reference_foreach            := Bind('git_reference_foreach', '16');
   git_reference_is_packed          := Bind('git_reference_is_packed', '4');
   git_reference_reload             := Bind('git_reference_reload', '4');
   git_reference_free               := Bind('git_reference_free', '4');

   git_remote_new                   := Bind('git_remote_new', '16');
   git_remote_load                  := Bind('git_remote_load', '12');
   git_remote_name                  := Bind('git_remote_name', '4');
   git_remote_url                   := Bind('git_remote_url', '4');
   git_remote_fetchspec             := Bind('git_remote_fetchspec', '4');
   git_remote_pushspec              := Bind('git_remote_pushspec', '4');
   git_remote_connect               := Bind('git_remote_connect', '8');
   git_remote_ls                    := Bind('git_remote_ls', '12');
   git_remote_download              := Bind('git_remote_download', '8');
   git_remote_connected             := Bind('git_remote_connected', '4');
   git_remote_disconnect            := Bind('git_remote_disconnect', '4');
   git_remote_free                  := Bind('git_remote_free', '4');
   git_remote_update_tips           := Bind('git_remote_update_tips', '4');
   git_remote_valid_url             := Bind('git_remote_valid_url', '4');

   git_repository_open              := Bind('git_repository_open', '8');
   git_repository_discover          := Bind('git_repository_discover', '20');
   git_repository_free              := Bind('git_repository_free', '4');
   git_repository_init              := Bind('git_repository_init', '12');
   git_repository_head              := Bind('git_repository_head', '8');
   git_repository_head_detached     := Bind('git_repository_head_detached', '4');
   git_repository_head_orphan       := Bind('git_repository_head_orphan', '4');
   git_repository_is_empty          := Bind('git_repository_is_empty', '4');
   git_repository_path              := Bind('git_repository_path', '4');
   git_repository_workdir           := Bind('git_repository_workdir', '4');
   git_repository_set_workdir       := Bind('git_repository_set_workdir', '8');
   git_repository_is_bare           := Bind('git_repository_is_bare', '4');
   git_repository_config            := Bind('git_repository_config', '8');
   git_repository_set_config        := Bind('git_repository_set_config', '8');
   git_repository_odb               := Bind('git_repository_odb', '8');
   git_repository_set_odb           := Bind('git_repository_set_odb', '8');
   git_repository_index             := Bind('git_repository_index', '8');
   git_repository_set_index         := Bind('git_repository_set_index', '8');

   git_revwalk_new                  := Bind('git_revwalk_new', '8');
   git_revwalk_reset                := Bind('git_revwalk_reset', '4');
   git_revwalk_push                 := Bind('git_revwalk_push', '8');
   git_revwalk_hide                 := Bind('git_revwalk_hide', '8');
   git_revwalk_next                 := Bind('git_revwalk_next', '8');
   git_revwalk_sorting              := Bind('git_revwalk_sorting', '8');
   git_revwalk_free                 := Bind('git_revwalk_free', '4');
   git_revwalk_repository           := Bind('git_revwalk_repository', '4');

   git_signature_new                := Bind('git_signature_new', '24');
   git_signature_now                := Bind('git_signature_now', '12');
   git_signature_dup                := Bind('git_signature_dup', '4');
   git_signature_free               := Bind('git_signature_free', '4');

   git_status_foreach               := Bind('git_status_foreach', '12');
   git_status_file                  := Bind('git_status_file', '12');
   git_status_should_ignore         := Bind('git_status_should_ignore', '12');

   git_tag_id                       := Bind('git_tag_id', '4');
   git_tag_target                   := Bind('git_tag_target', '8');
   git_tag_target_oid               := Bind('git_tag_target_oid', '4');
   git_tag_type                     := Bind('git_tag_type', '4');
   git_tag_name                     := Bind('git_tag_name', '4');
   git_tag_tagger                   := Bind('git_tag_tagger', '4');
   git_tag_message                  := Bind('git_tag_message', '4');
   git_tag_create                   := Bind('git_tag_create', '28');
   git_tag_create_frombuffer        := Bind('git_tag_create_frombuffer', '16');
   git_tag_create_lightweight       := Bind('git_tag_create_lightweight', '20');
   git_tag_delete                   := Bind('git_tag_delete', '8');
   git_tag_list                     := Bind('git_tag_list', '8');
   git_tag_list_match               := Bind('git_tag_list_match', '12');

   git_threads_init                 := Bind('git_threads_init', '0');
   git_threads_shutdown             := Bind('git_threads_shutdown', '0');

   git_tree_id                      := Bind('git_tree_id', '4');
   git_tree_entrycount              := Bind('git_tree_entrycount', '4');
   git_tree_entry_byname            := Bind('git_tree_entry_byname', '8');
   git_tree_entry_byindex           := Bind('git_tree_entry_byindex', '8');
   git_tree_entry_attributes        := Bind('git_tree_entry_attributes', '4');
   git_tree_entry_name              := Bind('git_tree_entry_name', '4');
   git_tree_entry_id                := Bind('git_tree_entry_id', '4');
   git_tree_entry_type              := Bind('git_tree_entry_type', '4');
   git_tree_entry_2object           := Bind('git_tree_entry_2object', '12');
   git_tree_create_fromindex        := Bind('git_tree_create_fromindex', '8');

   git_treebuilder_create           := Bind('git_treebuilder_create', '8');
   git_treebuilder_clear            := Bind('git_treebuilder_clear', '4');
   git_treebuilder_free             := Bind('git_treebuilder_free', '4');
   git_treebuilder_get              := Bind('git_treebuilder_get', '8');
   git_treebuilder_insert           := Bind('git_treebuilder_insert', '20');
   git_treebuilder_remove           := Bind('git_treebuilder_remove', '8');
   git_treebuilder_filter           := Bind('git_treebuilder_filter', '12');
   git_treebuilder_write            := Bind('git_treebuilder_write', '12');
   git_tree_get_subtree             := Bind('git_tree_get_subtree', '12');
   git_tree_walk                    := Bind('git_tree_walk', '16');

   // TODO : not exported?
//   gitwin_set_codepage              := Bind('gitwin_set_codepage', '');
//   gitwin_get_codepage              := Bind('gitwin_get_codepage', '');
//   gitwin_set_utf8                  := Bind('gitwin_set_utf8', '');
end;

function InitLibgit2: Boolean;
begin
  if libgit2 = 0 then
  begin
    libgit2 := LoadLibrary('git2.dll');
    if libgit2 > 0 then
      BindFuncs(true);
  end;

  Result := libgit2 > 0;
end;

procedure FreeLibgit2;
begin
  if libgit2 <> 0 then
  begin
    FreeLibrary(libgit2);
    libgit2 := 0;

    BindFuncs(false);
  end;
end;

initialization
  libgit2 := 0;
finalization
  FreeLibgit2;

end.

