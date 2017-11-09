#import <Appkit/Appkit.h>

#if __cplusplus
extern "C" {
#endif

const char* getClipboard(void) {
  NSString * contents = [[NSPasteboard generalPasteboard] stringForType:NSPasteboardTypeString];
  return [contents UTF8String];
}

void setClipboard(char* s) {
  NSString * nss = [NSString stringWithCString:s encoding:[NSString defaultCStringEncoding]];
  [[NSPasteboard generalPasteboard] clearContents];
  [[NSPasteboard generalPasteboard] setString:nss forType:NSPasteboardTypeString];
}

#if __cplusplus
}
#endif
