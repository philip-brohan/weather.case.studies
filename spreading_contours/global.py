# Global weather plot from 20CR2C - rotated pole
# Use Meteorographica weathermap functions

import matplotlib

from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
from matplotlib.figure import Figure

import iris
from iris.analysis.cartography import rotate_winds
import os
import numpy
import matplotlib.colors
from matplotlib.patches import Circle
import Meteorographica.data.twcr as twcr
import cartopy
import cartopy.mpl.geoaxes
import cartopy.crs as ccrs
import math
import pandas
import virtualtime # fixes datetime to work pre-1900
import datetime

import Meteorographica.weathermap as wm

# Specify the data to plot
year=1999
month=12
day=27
hour=6
member=1
dte=datetime.datetime(year,month,day,hour)

# set the region to plot
projection=ccrs.RotatedPole(pole_longitude=160.0,
                                pole_latitude=45.0,
                                central_rotated_longitude=-40.0)

# Define the page
extent=[-180.0,180.0,-90.0,90.0]
aspect=(extent[3]-extent[2])/(extent[1]-extent[0])
fig=Figure(figsize=(22,22*9/16),              # Width, Height (inches)
           dpi=100,
           facecolor=(0.88,0.88,0.88,1),
           edgecolor=None,
           linewidth=0.0,
           frameon=False,                # Don't draw a frame
           subplotpars=None,
           tight_layout=None)
# Attach a canvas
canvas=FigureCanvas(fig)
font = {'family' : 'sans-serif',
        'sans-serif' : 'Arial',
        'weight' : 'normal',
        'size'   : 12}
matplotlib.rc('font', **font)

## Axes to provide range and coordinate system - fill the fig.
ax = fig.add_axes([0,0,1,1],projection=projection)
ax.set_axis_off()
ax.set_extent(extent, crs=projection)
ax.set_aspect('auto')
matplotlib.rc('image',aspect='auto')

# Set the background colour
ax.background_patch.set_facecolor((0.88,0.88,0.88,1))

# Plot the land
land_img=ax.background_img(name='GreyT', resolution='low')

# Add a lat lon grid
#wm.add_grid(ax,sep_major=5,sep_minor=2.5,color=(0,0.3,0,0.2))

# Overplot the pressure as a spreading contour plot
prmsl=twcr.load('prmsl',year,month,day,hour,
                             version='4.5.2')
prmsl.data=prmsl.data/100 # To hPa
wm.plot_contour_spread(ax,prmsl,
                    levels=numpy.arange(870,1050,10),
                    resolution=0.25,
                    threshold=0.33,
                    line_threshold=10.0,
                    linewidths=3,
                    label=True,
                    scale=3,offset=0)


# Add the observations
#obs=twcr.load_observations_fortime(dte,version='4.5.1')
#wm.plot_obs(ax,obs,radius=0.2)

# Label the plot with a date
label=dte.strftime("%A %-d %B %Y at %-H GMT")
wm.plot_label(ax,label,facecolor=fig.get_facecolor(),
                  fontsize=11,x_fraction=0.99)

# Output as png with no border
fig.savefig('global.rotated.%04d-%02d-%02d:%02d.png' % (year,month,day,hour))
