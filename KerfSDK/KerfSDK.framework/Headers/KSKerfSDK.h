//
//  KSKerfSDK.h
//  KerfSDK
//
//  Created by kevin on 2015.04.02.
//  Copyright (c) 2015 Kerf Corp. All rights reserved.
//

#import <Foundation/Foundation.h>

static dispatch_queue_t _kerfAPIQueue;

@interface KSKerfSDK : NSObject

@property (nonatomic, assign) BOOL showTimingEnabled;

- (id)jsonObjectFromCall:(NSString *)call;
- (id)jsonObjectFromCall:(NSString *)call withArgumentArray:(NSArray *)args;
- (id)eval:(NSString *)call;
- (id)eval:(NSString *)call args:(id)firstArg, ... NS_REQUIRES_NIL_TERMINATION;

- (BOOL)isKerfErrorJSONObject:(id)arg;

- (NSString *)suggestedTableDirectoryPath;
- (NSString *)suggestedTableDirectoryPathWithoutMakingPath;

- (NSDate *)dateFromStringStamp:(NSString *)stamp;
- (NSString *)stringStampFromDate:(NSDate *)date;

@end
