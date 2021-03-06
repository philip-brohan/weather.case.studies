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
year=1903
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

# 

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

def _make_dummy(ax,resolution):

    extent=ax.get_extent()
    pole_latitude=ax.projection.proj4_params['o_lat_p']
    pole_longitude=ax.projection.proj4_params['lon_0']-180
    npg_longitude=ax.projection.proj4_params['o_lon_p']

    cs=iris.coord_systems.RotatedGeogCS(pole_latitude,
                                        pole_longitude,
                                        npg_longitude)
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
    return plot_cube

contour_colour_dict = {'red'  : ((0.0, 0.0, 0.0), 
                                        (1.0, 0.0, 0.0)), 
                              'blue': ((0.0, 0.45, 0.45), 
                                        (1.0, 0.45, 0.45)), 
                              'green' : ((0.0, 0.0, 0.0), 
                                        (1.0, 0.0, 0.0)), 
                              'alpha': ((0.0, 0.0, 0.0),
                                        (1.0, 0.55, 0.55)) 
} 
contour_cmap= matplotlib.colors.LinearSegmentedColormap('p_cmap',contour_colour_dict)

def plot_contour_spread(ax,prmsl,cmap,levels,scale=1,vmin=0,vmax=0.25,zorder=4):

    prmsl_m=prmsl.collapsed('member', iris.analysis.MEAN)
    prmsl_m.data=prmsl_m.data/100.0 # To hPa
    prmsl_s=prmsl.collapsed('member', iris.analysis.STD_DEV)
    prmsl_s.data=prmsl_s.data/100.0
    plot_cube=_make_dummy(ax_2c,0.1)
    prmsl_mu = prmsl_m.regrid(plot_cube,iris.analysis.Linear())
    prmsl_su = prmsl_s.regrid(plot_cube,iris.analysis.Linear())
    prmsl_u = prmsl_mu.copy()
    prmsl_u.data=prmsl_u.data*0.0
    prmsl_t = prmsl_mu.copy()
    prmsl_t.data=prmsl_u.data*0.0
    for level in levels:
        prmsl_t.data=scipy.stats.norm(prmsl_mu.data-level,prmsl_su.data).pdf(0)
        prmsl_u.data=numpy.maximum(prmsl_u.data,prmsl_t.data)

    lats = prmsl_u.coord('latitude').points
    lons = prmsl_u.coord('longitude').points
    u_img=ax_2c.pcolorfast(lons, lats, prmsl_u.data, cmap=contour_cmap,
                                vmin=0,vmax=0.25,zorder=zorder-1)
    # Mask out mean where uncertainties large
    prmsl_m.data[numpy.where(prmsl_s.data>7.5)]=numpy.nan
    CS=wm.plot_contour(ax_2c,prmsl_m,
                       levels=levels,
                       colors='black',
                       label=True,
                       linewidths=2,
                       zorder=zorder)

# load the 2c pressures
prmsl=twcr.load('prmsl',year,month,day,hour,
                                version='2c')
plot_contour_spread(ax_2c,prmsl,contour_cmap,numpy.arange(870,1050,10),
                    scale=1,vmin=0,vmax=0.25)

# 20CR2c label
wm.plot_label(ax_2c,'20CR 2c',
                     facecolor=fig.get_facecolor(),
                     x_fraction=0.02,
                     horizontalalignment='left')

# old-style panel

obs=twcr.load_observations_fortime(dte,version='2c')
wm.plot_obs(ax_3,obs,radius=0.1)

# load the V3 pressures
prmsl=twcr.load('prmsl',year,month,day,hour,
                                version='2c')

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
#prmsl_m.data[numpy.where(prmsl_s.data>3)]=numpy.nan
CS=wm.plot_contour(ax_3,prmsl_m,
                   levels=numpy.arange(870,1050,10),
                   colors='black',
                   label=True,
                   linewidths=2)

wm.plot_label(ax_3,'20CR v2c',
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
