# Global weather plot from 20CR2C
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
import IRData.twcr as twcr
import cartopy
import cartopy.crs as ccrs
import math
import pandas
#import virtualtime # fixes datetime to work pre-1900
import datetime

import Meteorographica as mg

# Specify the data to plot
year=1987
month=10
day=16
hour=6
member=1
dte=datetime.datetime(year,month,day,hour)

# set the region to plot
projection=ccrs.RotatedPole(pole_longitude=180.0, pole_latitude=90.0)

# Define the page
extent=[-180.0,180.0,-90.0,90.0]
aspect=(extent[3]-extent[2])/(extent[1]-extent[0])
fig=Figure(figsize=(22,22*aspect),              # Width, Height (inches)
           dpi=100,
           facecolor=(0.88,0.88,0.88,1),
           edgecolor=None,
           linewidth=0.0,
           frameon=False,                # Don't draw a frame
           subplotpars=None,
           tight_layout=None)
# Attach a canvas
canvas=FigureCanvas(fig)

resolution=0.25

## Axes to provide range and coordinate system - fill the fig.
ax = fig.add_axes([0,0,1,1],projection=projection)
ax.set_axis_off()
ax.set_extent(extent, crs=projection)

# Set the background colour
ax.background_patch.set_facecolor((0.88,0.88,0.88,1))

# Add a lat lon grid
mg.background.add_grid(ax,sep_major=5,sep_minor=2.5,color=(0,0.3,0,0.2))

# Plot the land
matplotlib.rc('image',aspect='auto') # Stop image over-riding aspect
land_img=ax.background_img(name='GreyT', resolution='low')

# Plot the precip
prate=twcr.load('prate',dte,version='2c')
pe=prate.extract(iris.Constraint(member=member))
prate_img=mg.precipitation.plot(ax,pe)

# Overplot the pressure as a contour plot
prmsl=twcr.load('prmsl',dte,version='2c')
pe=prmsl.extract(iris.Constraint(member=member))
pe.data=pe.data/100 # To hPa for labels
CS=mg.pressure.plot(ax,pe,type='contour',
                    levels=numpy.arange(870,1050,5),
                    colors='black',
                    fontsize=8,label=True)

# Overplot the wind vectors as a quiver plot
u=twcr.load('uwnd.10m',dte,version='2c')
ue=u.extract(iris.Constraint(member=member))
v=twcr.load('vwnd.10m',dte,version='2c')
ve=v.extract(iris.Constraint(member=member))
qv=mg.wind.plot(ax,ue,ve,scale=1,max_points=100000)

# Add the observations
obs=twcr.load_observations_fortime(dte,version='2c')
# Filter to those assimilated
obs_s=obs.loc[(obs['Assimilation.indicator']==1)].copy()
mg.observations.plot(ax,obs_s,radius=0.3)

# Label the plot with a date
label=dte.strftime("%A %-d %B %Y at %-H GMT")
mg.utils.plot_label(ax,label,
                    facecolor=fig.get_facecolor())

# Output as png with no border
fig.savefig('global.%04d-%02d-%02d:%02d.png' % (year,month,day,hour))
