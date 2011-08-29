#!/usr/bin/env python
# -*- coding: utf-8 -*-

class ApacheCalculater(object):

    def __init__(self):
        pass

    @classmethod
    def calculateApacheIpHits(cls, log):
        ipHitListing = {}
        contents = open(log, "r")
        for line in contents:
            ip = line.split(" ", 1)[0]
            if 6 < len(ip) <= 15:
                ipHitListing[ip] = ipHitListing.get(ip, 0) + 1
        return sorted(ipHitListing.items(),
                reverse=True,
                key=lambda x:x[1])

    @classmethod
    def clientCachePercentage(cls, log):
        contents = open(log, "r")
        totalRequests = 0
        cachedRequests = 0
        for line in contents:
            totalRequests += 1
            if line.split(" ")[8] == "304":
                cachedRequests += 1
        return float(100*cachedRequests)/totalRequests

