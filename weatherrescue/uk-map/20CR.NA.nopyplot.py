# UK region weather plot from 20CR2C
# Pure OO version - no pyplot.

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
scale=30
resolution=0.25
extent=[scale*-1,scale,scale*-1/math.sqrt(2),scale/math.sqrt(2)]

## Axes to provide range and coordinate system - fill the fig.
ax = fig.add_axes([0,0,1,1],projection=projection)
ax.set_axis_off()
ax.set_extent(extent, crs=projection)

# Set the background colour
ax.background_patch.set_facecolor((0.88,0.88,0.88,1))

# Add a lat lon grid
gl_minor=ax.gridlines(linestyle='-',linewidth=0.2,color=(0,0.50,0,0.3),zorder=0)
gl_minor.xlocator = matplotlib.ticker.FixedLocator(numpy.arange(-180,180,0.5))
gl_minor.ylocator = matplotlib.ticker.FixedLocator(numpy.arange(-90,90,0.5))
gl_major=ax.gridlines(linestyle='-',linewidth=1,color=(0,0.50,0,0.3),zorder=0)
gl_major.xlocator = matplotlib.ticker.FixedLocator(numpy.arange(-180,180,2))
gl_major.ylocator = matplotlib.ticker.FixedLocator(numpy.arange(-90,90,2))

# Plot the land
land_img=ax.background_img(name='GreyT', resolution='low')

# Make a dummy cube to use as a plot grid
cs=iris.coord_systems.RotatedGeogCS(37.5,177.5)
projection_iris=cs
lat_values=numpy.arange(extent[2]-2,extent[3]+2,resolution)
latitude = iris.coords.DimCoord(lat_values,
                                standard_name='latitude',
                                units='degrees_north',
                                coord_system=cs)
lon_values=numpy.arange(extent[0]-2,extent[1]+2,resolution)
longitude = iris.coords.DimCoord(lon_values,
                                standard_name='longitude',
                                units='degrees_east',
                                coord_system=cs)
dummy_data = numpy.zeros((len(lat_values), len(lon_values)))
plot_cube = iris.cube.Cube(dummy_data,
                           dim_coords_and_dims=[(latitude, 0),
                                                (longitude, 1)])

# Overplot the precip as a partially-transparent colour mesh
c_dict = {'red'  : ((0.0, 0.0, 0.0), 
                    (1.0, 0.0, 0.0)), 
          'green': ((0.0, 0.3, 0.3), 
                    (1.0, 0.3, 0.3)), 
          'blue' : ((0.0, 0.0, 0.0), 
                    (1.0, 0.0, 0.0)), 
          'alpha': ((0.0, 0.0, 0.0),
                    (0.2, 0.0, 0.0),
                    (1.0, 0.95, 0.95)) 
} 
p_cmap= matplotlib.colors.LinearSegmentedColormap('p_cmap',c_dict)
prate=twcr.get_slice_at_hour('prate',year,month,day,hour,
                             version='3.5.1',type='ensemble')
pe=prate.extract(iris.Constraint(member=member))
prate_p = pe.regrid(plot_cube,iris.analysis.Linear())
prate_p.data=numpy.sqrt(prate_p.data)
#prate_p.data[numpy.where(prate_p.data>0.025)]=0.025
lats=numpy.arange(extent[2]-2-resolution/2,extent[3]+2+resolution/2,resolution)
lons=numpy.arange(extent[0]-2-resolution/2,extent[1]+2+resolution/2,resolution)
prate_img=ax.pcolorfast(lons, lats, prate_p.data, cmap=p_cmap,
                        vmin=0,vmax=0.025,zorder=4)

# Overplot the pressure as a contour plot
prmsl=twcr.get_slice_at_hour('prmsl',year,month,day,hour,
                             version='3.5.1',type='ensemble')
pe=prmsl.extract(iris.Constraint(member=member))
prmsl_p = pe.regrid(plot_cube,iris.analysis.Linear())
lats = prate_p.coord('latitude').points
lons = prate_p.coord('longitude').points
lons,lats = numpy.meshgrid(lons,lats)
CS=ax.contour(lons, lats, prmsl_p.data/100,
                               colors='black',
                               linewidths=0.5,
                               levels=numpy.arange(870,1050,3),
                               zorder=4)

# Label the contours
cl=ax.clabel(CS, inline=1, fontsize=12, fmt='%d',zorder=5.1)

# Overplot the wind vectors as a quiver plot
u=twcr.get_slice_at_hour('uwnd.10m',year,month,day,hour,
                             version='3.5.1',type='ensemble')
ue=u.extract(iris.Constraint(member=member))
v=twcr.get_slice_at_hour('vwnd.10m',year,month,day,hour,
                             version='3.5.1',type='ensemble')
ve=v.extract(iris.Constraint(member=member))
rw=rotate_winds(ue,ve,projection_iris)
u_p = rw[0].regrid(plot_cube,iris.analysis.Linear())
v_p = rw[1].regrid(plot_cube,iris.analysis.Linear())
lats=numpy.arange(extent[2],extent[3],resolution*4)
lons=numpy.arange(extent[0],extent[1],resolution*4)
lons,lats = numpy.meshgrid(lons,lats)
lons=lons.flatten()
lats=lats.flatten()
u_interpolator = iris.analysis.Linear().interpolator(u_p, 
                                    ['latitude', 'longitude'])
v_interpolator = iris.analysis.Linear().interpolator(v_p, 
                                    ['latitude', 'longitude'])
u_i=numpy.zeros(lons.size)
v_i=numpy.zeros(lons.size)
for i in range(lons.size):
    u_i[i]=u_interpolator([lats[i],lons[i]]).data*-1
    v_i[i]=v_interpolator([lats[i],lons[i]]).data*-1
qv=ax.quiver(lons,lats,u_i,v_i,
                            headwidth=1,
                            color=(0,0,0,0.25),
                            zorder=4.5)

# Add the observations
obs=twcr.get_obs_1file(year,month,day,hour,'3.5.1')
# Filter to those assimilated and near the UK
obs_s=obs.loc[(obs['Assimilation.indicator']==1) &
              ((obs['Latitude']>0) & (obs['Latitude']<90)) &
              ((obs['Longitude']>240) | (obs['Longitude']<100))].copy()
# Rotate positions into plot coordinates
rp=projection.transform_points(ccrs.PlateCarree(),
                               obs_s['Longitude'].values,
                               obs_s['Latitude'].values)
obs_s['Longitude']=rp[:,0]
obs_s['Latitude']=rp[:,1]

# Plot each ob as a circle
for ob in obs_s.itertuples():
    ax.add_patch(Circle((getattr(ob, "Longitude"),
                         getattr(ob, "Latitude")),
                        radius=0.1,
                        facecolor='yellow',
                        edgecolor='black',
                        alpha=0.85,
                        zorder=2.5))

# Label the plot with a date
ax.text(extent[0]*0.02+extent[1]*0.98,
        extent[2]*0.98+extent[3]*0.02,
        dte.strftime("%A %-d %B %Y at %-H GMT"),
        horizontalalignment='right',
        verticalalignment='bottom',
        color='black',
        bbox=dict(facecolor=fig.get_facecolor(),
                  edgecolor=(1,1,1,0),
                  boxstyle='round',
                  pad=0.5),
        size=12,
        clip_on=True,
        zorder=5.5)

# Output as png with no border
extent = ax.get_window_extent().transformed(fig.dpi_scale_trans.inverted())
fig.savefig('NA.%04d-%02d-%02d:%02d.nopyplot.png' % (year,month,day,hour))
