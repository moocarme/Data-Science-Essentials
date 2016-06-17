# -*- coding: utf-8 -*-
"""
Created on Thu Jun 16 19:13:10 2016

@author: mmoocarme
"""

forecasts = open("forecasts_wogtrend.csv", "r").readlines()

old_value = 1
new_list = []
for f in forecasts[1:]:
    strpf = f.replace('"','').strip()
    new_str = "%s,%s\n" % (strpf, old_value)
    newspl = new_str.strip().split(",")
    final_str = "%s,%s\n" % (newspl[0], newspl[2])
    final_str = final_str.replace('"','')
    old_value = f.strip().split(',')[1]
    new_list.append(final_str)

out = open("forecasts_new_wogtrend.csv", "w")
for n in new_list:
    out.write(n)
out.close()