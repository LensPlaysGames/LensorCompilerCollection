#ifndef LCC_BASE_PLATFORM_HH
#define LCC_BASE_PLATFORM_HH

#if defined(__linux__)
#    define LCC_LINUX 1
#    if defined(__GLIBC__) && ! defined(__UCLIBC__)
#        define LCC_LINUX_GLIBC 1
#    endif
#endif

#if defined(__APPLE__)
#    define LCC_APPLE 1
#endif

#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__) || defined(__NT__)
#    define LCC_WINDOWS 1
#endif

#endif /* LCC_BASE_PLATFORM_HH */
