# Hong Kong region weather plot 
# Compare pressures from 20CRV3 and 20CRV2c

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

# Select Nancy obs only
def get_nancy(obs):
   return obs[obs.Name=='NANCY']

# Show obs in use
def plot_obs_thistime(ax,key_time,version,
             ffn=None,
             obs_projection=ccrs.PlateCarree(),
             lat_label='Latitude',lon_label='Longitude',
             radius=0.1,
             facecolor='yellow',
             edgecolor='black',
             zorder=2.5):

    # Obs for previous/current step
    kto=key_time-datetime.timedelta(hours=key_time.hour%6)
    obs=twcr.load_observations_1file(kto.year,
                                     kto.month,
                                     kto.day,
                                     kto.hour,
                                     version=version)
    if ffn is not None:
        obs=ffn(obs)
    if not obs.empty:
        alpha=1
        if version=='2c':
            alpha=(6-key_time.hour%6-key_time.minute/60.0)/6.0
        else:
            if key_time.hour%6>3:
                alpha=(3-key_time.hour%3-key_time.minute/60.0)/3.0
        wm.plot_obs(ax,obs,
                    radius=radius,
                    facecolor=facecolor,
                    edgecolor=edgecolor,
                    alpha=alpha,
                    zorder=zorder)

    # Obs next step
    kto=kto+datetime.timedelta(hours=6)
    obs=twcr.load_observations_1file(kto.year,
                                     kto.month,
                                     kto.day,
                                     kto.hour,
                                     version=version)
    if ffn is not None:
        obs=ffn(obs)
    if not obs.empty:
        alpha=0
        if version=='2c':
            alpha=1+(key_time.hour%6+key_time.minute/60.0-6)/6.0
        else:
            if key_time.hour%6>3:
                alpha=1+(key_time.hour%3+key_time.minute/60.0-3)/3.0
        if alpha>0:
            wm.plot_obs(ax,obs,
                        radius=radius,
                        facecolor=facecolor,
                        edgecolor=edgecolor,
                        alpha=alpha,
                        zorder=zorder)


# Date to show
year=1961
month=9
day=16
hour=12
dte=datetime.datetime(year,month,day,hour)

# Landscape page
fig=Figure(figsize=(22,22/math.sqrt(2)),  # Width, Height (inches)
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
extent=[scale*-1,scale,scale*-1*math.sqrt(2),scale*math.sqrt(2)]

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
plot_obs_thistime(ax_2c,dte,version='2c',radius=0.15)
# Highlight the Nancy obs
plot_obs_thistime(ax_2c,dte,version='2c',ffn=get_nancy,
                  radius=0.25,facecolor='red',zorder=12.6)

# load the 2c pressures
prmsl=twcr.load('prmsl',year,month,day,hour,
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
CS=wm.plot_contour(ax_2c,prmsl_m,
                   levels=numpy.arange(870,1050,10),
                   colors='black',
                   label=True,
                   linewidths=2)

# 20CR2c label
wm.plot_label(ax_2c,'20CR 2c',
                     facecolor=fig.get_facecolor(),
                     x_fraction=0.02,
                     horizontalalignment='left')

# V3 panel

# Add the observations from 3
plot_obs_thistime(ax_3,dte,version='4.5.1',radius=0.15)
# Highlight the Nancy obs
plot_obs_thistime(ax_3,dte,version='4.5.1',ffn=get_nancy,
                  radius=0.25,facecolor='red',zorder=12.6)
obs=twcr.load_observations(dte-datetime.timedelta(hours=24),
                           dte,
                           version='4.5.1')

# load the V3 pressures
prmsl=twcr.load('prmsl',year,month,day,hour,
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
CS=wm.plot_contour(ax_3,prmsl_m,
                   levels=numpy.arange(870,1050,10),
                   colors='black',
                   label=True,
                   linewidths=2)

wm.plot_label(ax_3,'20CR v3',
                     facecolor=fig.get_facecolor(),
                     x_fraction=0.02,
                     horizontalalignment='left')

wm.plot_label(ax_3,
              '%04d-%02d-%02d:%02d' % (year,month,day,hour),
              facecolor=fig.get_facecolor(),
              x_fraction=0.98,
              horizontalalignment='right')

# Output as png
fig.savefig('V3vV2c_Typhoon_Nancy_%04d%02d%02d%02d.png' % 
                                  (year,month,day,hour))
