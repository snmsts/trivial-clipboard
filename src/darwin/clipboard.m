#import <Appkit/Appkit.h>

#if __cplusplus
extern "C" {
#endif

const char* getClipboard(void) {
  return [[[NSPasteboard generalPasteboard] stringForType:NSPasteboardTypeString] UTF8String];
}

void setClipboard(const char* s) {
  [[NSPasteboard generalPasteboard] clearContents];
  [[NSPasteboard generalPasteboard]
   setString:[NSString stringWithCString:s encoding:[NSString defaultCStringEncoding]]
   forType:NSPasteboardTypeString];
}

#if __cplusplus
}
#endif
