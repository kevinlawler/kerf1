//
//  DemoCode.m
//  KerfDemo
//
//  Created by kevin on 2015.04.02.
//  Copyright (c) 2015 Kerf Corp. All rights reserved.
//

#import "DemoCode.h"

@implementation DemoCode

+ (void)runDemoCode
{
    NSLog(@"--------------------------------------------------");

#ifdef _LP64
    NSLog(@"64-bit Code");
#else
    NSLog(@"32-bit Code");
#endif
    
    KSKerfSDK *kerf = [KSKerfSDK new];

    kerf.showTimingEnabled = YES;
    
    NSLog(@"--------------------------------------------------");
    NSLog(@"%@", [kerf jsonObjectFromCall:@"1+1"]);
    
    NSLog(@"%@", [kerf jsonObjectFromCall:@"sum(range(100))"]);
    
    NSLog(@"%@", [kerf jsonObjectFromCall:@"(100*101)/2"]);
    
    NSLog(@"%@", [kerf jsonObjectFromCall:@"[$1, $2, $1+$2]" withArgumentArray:@[@1.2, @2.3]]);
    
    id intentional_error = [kerf jsonObjectFromCall:@" 1 + 'abc' //intentional error"];
    
    NSLog(@"Correct error-handling? (Expected YES): %@", [kerf isKerfErrorJSONObject:intentional_error] ? @"YES":@"NO");

    NSLog(@"--------------------------------------------------");

    NSLog(@"Working with mapped table. Note: running this without cleaning the app folder will reappend, causing the table to grow.");

    NSString *path = [[kerf suggestedTableDirectoryPath] stringByAppendingPathComponent:@"a.table"];
    
    [kerf jsonObjectFromCall:@"a: open_table($1);" withArgumentArray:@[path]];
    
    [kerf jsonObjectFromCall:@"insert into a values {{time:now(), heartrate:60 + rand(100.0), velocity:rand(100.0)}}"];
    
    id json_a = [kerf jsonObjectFromCall:@"a"];
    
    if([kerf isKerfErrorJSONObject:json_a])
    {
        NSLog(@"Error in table generation: %@", json_a);
    }

    NSLog(@"--------------------------------------------------");

    id json = nil;

    json = [kerf eval:@"n: $1" args:@10000, nil];
    json = [kerf eval:@"ids: range(1, n+1)"];
    json = [kerf eval:@"stamps: plus(NOW(), 1s + mapright range(n))"];
    json = [kerf eval:@"heartrates: 80 + rand(n, 100.0)"];
    json = [kerf eval:@"labels: range(6)"];
    json = [kerf eval:@"lanes: take(n, join(labels, reverse labels))"];
    json = [kerf eval:@"running: {{id: ids, stamp: stamps, heartrate: heartrates, lane: lanes}}"];
    
    NSLog(@"%@", [kerf eval:@"select avg(heartrate) from running where heartrate > 100 group by lane"]);
    NSLog(@"%@", [kerf eval:@"select max(heartrate) from running where lane = 2"]);

    NSLog(@"--------------------------------------------------");

    NSLog(@"\n");
    
}

@end
