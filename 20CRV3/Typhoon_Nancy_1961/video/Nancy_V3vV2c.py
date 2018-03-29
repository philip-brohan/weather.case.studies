#!/usr/bin/env python

# Hong Kong region weather plot 
# Compare pressures from 20CRV3 and 20CRV2c
# Video version.

import os
import math
import datetime
import numpy
import pandas

import iris
import iris.analysis

import matplotlib
from matplotlib.backends.backend_agg import \
             FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure

import cartopy
import cartopy.crs as ccrs

import Meteorographica.weathermap as wm
import Meteorographica.data.twcr as twcr

# Get the datetime to plot from commandline arguments
import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--year", help="Year",
                    type=int,required=True)
parser.add_argument("--month", help="Integer month",
                    type=int,required=True)
parser.add_argument("--day", help="Day of month",
                    type=int,required=True)
parser.add_argument("--hour", help="Time of day (0 to 23.99)",
                    type=float,required=True)
parser.add_argument("--opdir", help="Directory for output files",
                    default="%s/images/Typhoon_Nancy" % \
                                           os.getenv('SCRATCH'),
                    type=str,required=False)
args = parser.parse_args()
if not os.path.isdir(args.opdir):
    os.makedirs(args.opdir)

dte=datetime.datetime(args.year,args.month,args.day,
                      int(args.hour),int(args.hour%1*60))

# Vary the obs plotted to show how close to time 
def plot_obs_bytime(ax,obs,key_time,
             obs_projection=ccrs.PlateCarree(),
             lat_label='Latitude',lon_label='Longitude',
             radius=0.1,
             facecolor='yellow',
             edgecolor='black',
             zorder=2.5):
    rp=ax.projection.transform_points(obs_projection,
                                   obs[lon_label].values,
                                   obs[lat_label].values)
    new_longitude=rp[:,0]
    new_latitude=rp[:,1]

    dtm=pandas.to_datetime(obs.UID.str.slice(0,10),format="%Y%m%d%H")
    # Plot each ob as a circle
    for i in range(0,len(new_longitude)):
        # Transparency depends on proximity to plot time
        alpha=1
        dth=(key_time-dtm.iloc[i]).total_seconds()/3600
        if dth>9 or dth<-3: continue
        if dth<0:
            alpha=1+dth/3.0
        if dth>6:
            alpha=1-(dth-6)/3.0
        ax.add_patch(matplotlib.patches.Circle((new_longitude[i],
                                                new_latitude[i]),
                                                radius=radius,
                                                facecolor=facecolor,
                                                edgecolor=edgecolor,
                                                alpha=alpha,
                                                zorder=zorder))

# HD video size 1920x1080
aspect=16.0/9.0
fig=Figure(figsize=(10.8*aspect,10.8),  # Width, Height (inches)
           dpi=100,
           facecolor=(0.88,0.88,0.88,1),
           edgecolor=None,
           linewidth=0.0,
           frameon=False,
           subplotpars=None,
           tight_layout=None)
canvas=FigureCanvas(fig)

# UK-centred projection
projection=ccrs.RotatedPole(pole_longitude=320, pole_latitude=56)
scale=30
extent=[scale*-1*aspect/2,scale*aspect/2,scale*-1,scale]

# Two side-by-side plots
ax_2c=fig.add_axes([0.01,0.01,0.485,0.98],projection=projection)
ax_2c.set_axis_off()
ax_2c.set_extent(extent, crs=projection)
ax_3=fig.add_axes([0.505,0.01,0.485,0.98],projection=projection)
ax_3.set_axis_off()
ax_3.set_extent(extent, crs=projection)

# Background, grid and land for both
ax_2c.background_patch.set_facecolor((0.88,0.88,0.88,1))
ax_3.background_patch.set_facecolor((0.88,0.88,0.88,1))
wm.add_grid(ax_2c)
wm.add_grid(ax_3)
land_img_2c=ax_2c.background_img(name='GreyT', resolution='low')
land_img_3=ax_3.background_img(name='GreyT', resolution='low')

# Add the observations from 2c
obs=twcr.load_observations(dte-datetime.timedelta(hours=10),
                           dte+datetime.timedelta(hours=4),
                           version='2c')
plot_obs_bytime(ax_2c,obs,dte,radius=0.15)
obs=obs[obs.Name=='NANCY']
if not obs.empty:
   plot_obs_bytime(ax_2c,obs,dte,radius=0.25,
                    facecolor='red',zorder=12.6)

# load the 2c pressures
prmsl=twcr.load('prmsl',args.year,args.month,args.day,args.hour,
                                version='2c')

# For each ensemble member, make a contour plot
for m in range(1, 57):
    prmsl_e=prmsl.extract(iris.Constraint(member=m))
    prmsl_e.data=prmsl_e.data/100 # To hPa
    CS=wm.plot_contour(ax_2c,prmsl_e,
                   levels=numpy.arange(870,1050,10),
                   colors='blue',
                   label=False,
                   linewidths=0.1)

# Add the ensemble mean - with labels
prmsl_m=prmsl.collapsed('member', iris.analysis.MEAN)
prmsl_m.data=prmsl_m.data/100 # To hPa
prmsl_s=prmsl.collapsed('member', iris.analysis.STD_DEV)
prmsl_s.data=prmsl_s.data/100
# Mask out mean where uncertainties large
prmsl_m.data[numpy.where(prmsl_s.data>3)]=numpy.nan
CS=wm.plot_contour(ax_2c,prmsl_m,
                   levels=numpy.arange(870,1050,10),
                   colors='black',
                   label=False,
                   linewidths=2)

# 20CR2c label
wm.plot_label(ax_2c,'20CR v2c',
                     facecolor=fig.get_facecolor(),
                     x_fraction=0.02,
                     horizontalalignment='left')

# V3 panel

# Add the observations from 3
obs=twcr.load_observations(dte-datetime.timedelta(hours=20),
                           dte+datetime.timedelta(hours=10),
                           version='4.5.1')
plot_obs_bytime(ax_3,obs,dte,radius=0.15)
obs=obs[obs.Name=='NANCY']
if not obs.empty:
   plot_obs_bytime(ax_3,obs,dte,radius=0.25,
                   facecolor='red',zorder=12.6)

# load the V3 pressures
prmsl=twcr.load('prmsl',args.year,args.month,args.day,args.hour,
                                version='4.5.1')

# For each ensemble member, make a contour plot
for m in range(1,57): # Same number as 2c
    prmsl_e=prmsl.extract(iris.Constraint(member=m))
    prmsl_e.data=prmsl_e.data/100 # To hPa
    CS=wm.plot_contour(ax_3,prmsl_e,
                   levels=numpy.arange(870,1050,10),
                   colors='blue',
                   label=False,
                   linewidths=0.1)

# Add the ensemble mean - with labels
prmsl_m=prmsl.collapsed('member', iris.analysis.MEAN)
prmsl_m.data=prmsl_m.data/100 # To hPa
prmsl_s=prmsl.collapsed('member', iris.analysis.STD_DEV)
prmsl_s.data=prmsl_s.data/100
# Mask out mean where uncertainties large
prmsl_m.data[numpy.where(prmsl_s.data>3)]=numpy.nan
CS=wm.plot_contour(ax_3,prmsl_m,
                   levels=numpy.arange(870,1050,10),
                   colors='black',
                   label=False,
                   linewidths=2)

wm.plot_label(ax_3,'20CR v3',
                     facecolor=fig.get_facecolor(),
                     x_fraction=0.02,
                     horizontalalignment='left')

wm.plot_label(ax_3,
              ('%04d-%02d-%02d:%02d' % 
               (args.year,args.month,args.day,args.hour)),
              facecolor=fig.get_facecolor(),
              x_fraction=0.98,
              horizontalalignment='right')

# Output as png
fig.savefig('%s/V3vV2c_Typhoon_Nancy_%04d%02d%02d%02d%02d.png' % 
               (args.opdir,args.year,args.month,args.day,
                           int(args.hour),int(args.hour%1*60)))
