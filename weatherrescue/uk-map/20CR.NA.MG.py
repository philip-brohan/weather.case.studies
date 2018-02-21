# UK region weather plot from 20CR2C
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
import cartopy.crs as ccrs
import math
import pandas
import virtualtime # fixes datetime to work pre-1900
import datetime

import Meteorographica.weathermap as wm

# Specify the data to plot
year=1987
month=10
day=16
hour=6
member=1
dte=datetime.datetime(year,month,day,hour)

# set the region to plot
projection=ccrs.RotatedPole(pole_longitude=177.5, pole_latitude=37.5)

# Define the page
fig=Figure(figsize=(22,22/math.sqrt(2)),              # Width, Height (inches)
           dpi=100,
           facecolor=(0.88,0.88,0.88,1),
           edgecolor=None,
           linewidth=0.0,
           frameon=False,                # Don't draw a frame
           subplotpars=None,
           tight_layout=None)
# Attach a canvas
canvas=FigureCanvas(fig)

# Extent - in rotated lat lon coordinates
scale=40
resolution=0.25
extent=[scale*-1,scale,scale*-1/math.sqrt(2),scale/math.sqrt(2)]

## Axes to provide range and coordinate system - fill the fig.
ax = fig.add_axes([0,0,1,1],projection=projection)
ax.set_axis_off()
ax.set_extent(extent, crs=projection)

# Set the background colour
ax.background_patch.set_facecolor((0.88,0.88,0.88,1))

# Add a lat lon grid
wm.add_grid(ax)

# Plot the land
land_img=ax.background_img(name='GreyT', resolution='low')

# Plot the precip
prate=twcr.get_slice_at_hour('prate',year,month,day,hour,
                             version='3.5.1',type='ensemble')
pe=prate.extract(iris.Constraint(member=member))
prate_img=wm.plot_cmesh(ax,pe)

# Overplot the pressure as a contour plot
prmsl=twcr.get_slice_at_hour('prmsl',year,month,day,hour,
                             version='3.5.1',type='ensemble')
pe=prmsl.extract(iris.Constraint(member=member))
pe.data=pe.data/100 # To hPa for labels
CS=wm.plot_contour(ax,pe,
                   levels=numpy.arange(870,1050,3),label=True)

# Overplot the wind vectors as a quiver plot
u=twcr.get_slice_at_hour('uwnd.10m',year,month,day,hour,
                             version='3.5.1',type='ensemble')
ue=u.extract(iris.Constraint(member=member))
v=twcr.get_slice_at_hour('vwnd.10m',year,month,day,hour,
                             version='3.5.1',type='ensemble')
ve=v.extract(iris.Constraint(member=member))
qv=wm.plot_quiver(ax,ue,ve,scale=0.5)

# Add the observations
obs=twcr.get_obs_1file(year,month,day,hour,'3.5.1')
# Filter to those assimilated and near the UK
obs_s=obs.loc[(obs['Assimilation.indicator']==1) &
              ((obs['Latitude']>0) & (obs['Latitude']<90)) &
              ((obs['Longitude']>240) | (obs['Longitude']<100))].copy()
wm.plot_obs(ax,obs_s)

# Label the plot with a date
label=dte.strftime("%A %-d %B %Y at %-H GMT")
wm.plot_label(ax,label,facecolor=fig.get_facecolor())

# Output as png with no border
fig.savefig('NA.%04d-%02d-%02d:%02d.MG.png' % (year,month,day,hour))
