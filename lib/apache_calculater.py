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
        return ipHitListing

