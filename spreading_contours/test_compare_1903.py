# UK region weather plot 
# Test the contour_spread plot

import math
import datetime
import numpy
import pandas
import scipy.stats

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

# Date to show
year=1953
month=2
day=27
hour=06
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
projection=ccrs.RotatedPole(pole_longitude=180, pole_latitude=35)
scale=15
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
obs=twcr.load_observations_fortime(dte,version='2c')
wm.plot_obs(ax_2c,obs,radius=0.1)

# load the 2c pressures
prmsl=twcr.load('prmsl',year,month,day,hour,
                                version='2c')
prmsl.data=prmsl.data/100.0 # To hPa
wm.plot_contour_spread(ax_2c,prmsl,
                    levels=numpy.arange(870,1050,10),
                    resolution=0.1,
                    threshold=0.33,
                    line_threshold=7,
                    linewidths=2,
                    label=True,
                    scale=3,offset=0)

# 20CR2c label
wm.plot_label(ax_2c,'20CR 2c',
                     facecolor=fig.get_facecolor(),
                     x_fraction=0.02,
                     horizontalalignment='left')

# old-style panel

obs=twcr.load_observations_fortime(dte,version='4.5.1')
wm.plot_obs(ax_3,obs,radius=0.1)

# load the V3 pressures
prmsl=twcr.load('prmsl',year,month,day,hour,
                                version='4.5.1')
prmsl.data=prmsl.data/100.0 # To hPa

wm.plot_contour_spread(ax_3,prmsl,
                    levels=numpy.arange(870,1050,10),
                    resolution=0.1,
                    threshold=0.05,
                    line_threshold=7,
                    linewidths=2,
                    label=True,
                    scale=1,offset=0)


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
fig.savefig('V3vV2c_%04d%02d%02d%02d.png' % 
                                  (year,month,day,hour))
