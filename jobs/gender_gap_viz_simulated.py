import pandas as pd
import numpy as np
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import os
import matplotlib.cm as cm
import matplotlib.colors as mcolors
import warnings
warnings.filterwarnings('ignore')

# Function to load data - using sample data since pyreadr installation has issues
def load_data():
    print("Creating sample data for demonstration")
    
    # Create sample data based on the patterns in viz_apctrends.R
    years = range(1982, 2021)
    birthyears = range(1940, 1991)
    
    data = []
    
    # Generate sample data
    for year in years:
        for birthyear in birthyears:
            age = year - birthyear
            if 25 <= age <= 55:  # Only include ages 25-55 as in the R code
                # Create gender-specific wages
                # Base wage that increases over time
                base_wage = 10 + (year - 1982) * 0.5
                
                # Age effect (wage increases with age)
                age_effect = (age - 25) * 0.3
                
                # Cohort effect (younger cohorts have higher starting wages)
                cohort_effect = (birthyear - 1940) * 0.1
                
                # Calculate male and female wages with a gender gap that:
                # - Decreases over time (year effect)
                # - Is larger for older cohorts (cohort effect)
                # - Increases with age (age effect)
                year_effect = 0.3 - (year - 1982) * 0.005
                cohort_gap_effect = (1990 - birthyear) * 0.003
                age_gap_effect = (age - 25) * 0.002
                
                gender_gap = max(0.05, min(0.4, year_effect + cohort_gap_effect + age_gap_effect))
                
                # Add some random variation
                gender_gap += np.random.normal(0, 0.01)
                
                # Calculate wages
                male_wage = base_wage + age_effect + cohort_effect
                female_wage = male_wage * (1 - gender_gap)
                
                # Add to data
                for gender in [0, 1]:  # 0 for male, 1 for female
                    wage = female_wage if gender == 1 else male_wage
                    weight = np.random.uniform(0.5, 1.5)  # Random weight
                    
                    data.append({
                        'YEAR': year,
                        'BIRTHYEAR': birthyear,
                        'FEMALE': gender,
                        'EARNHRLY_MAIN': wage,
                        'EARNWT': weight,
                        'AGE': age
                    })
    
    print(f"Sample data created with {len(data)} records")
    return pd.DataFrame(data)

# Process data similar to the R code
def process_data(df):
    print("Processing data...")
    
    # Group by year, birth year, and gender to calculate mean wages
    means = df.groupby(['YEAR', 'BIRTHYEAR', 'FEMALE']).apply(
        lambda x: pd.Series({
            'wage': np.average(x['EARNHRLY_MAIN'], weights=x['EARNWT'])
        })
    ).reset_index()
    
    # Pivot to get male and female wages in separate columns
    means_wide = means.pivot_table(
        index=['YEAR', 'BIRTHYEAR'], 
        columns='FEMALE', 
        values='wage'
    ).reset_index()
    
    # Rename columns
    means_wide.columns.name = None
    means_wide = means_wide.rename(columns={0: 'Men', 1: 'Women'})
    
    # Calculate ratio and gender gap
    means_wide['ratio'] = means_wide['Women'] / means_wide['Men']
    means_wide['gender_gap'] = (1 - means_wide['ratio']) * 100  # Convert to percentage
    
    # Add age column
    means_wide['AGE'] = means_wide['YEAR'] - means_wide['BIRTHYEAR']
    
    print(f"Data processed: {len(means_wide)} year-cohort combinations")
    return means_wide

# Create a color scale legend for birth years
def create_colorscale_legend(min_birth_year, max_birth_year):
    # Create a continuous color scale
    plasma = cm.get_cmap('plasma')
    
    # Create a range of birth years for the color scale
    birth_years = list(range(min_birth_year, max_birth_year + 1, 5))  # Every 5 years
    
    # Create color scale annotations
    colorscale_annotations = []
    for birthyear in birth_years:
        color_val = (birthyear - min_birth_year) / (max_birth_year - min_birth_year)
        color = mcolors.rgb2hex(plasma(color_val))
        
        colorscale_annotations.append(
            dict(
                x=1.05,  # Moved further to the right
                y=(birthyear - min_birth_year) / (max_birth_year - min_birth_year),
                xref='paper',
                yref='paper',
                text=str(birthyear),
                showarrow=False,
                font=dict(size=10),
                align='left'
            )
        )
    
    # Create color scale shapes
    colorscale_shapes = []
    for i in range(100):
        y_pos = i / 100
        color_val = y_pos
        color = mcolors.rgb2hex(plasma(color_val))
        
        colorscale_shapes.append(
            dict(
                type='rect',
                xref='paper',
                yref='paper',
                x0=1.03,  # Moved further to the right
                y0=y_pos,
                x1=1.04,  # Moved further to the right
                y1=y_pos + 0.01,
                fillcolor=color,
                line=dict(width=0)
            )
        )
    
    return colorscale_shapes, colorscale_annotations

# Create period visualization (by year)
def create_period_visualization(data, years, birthyears):
    print("Creating period visualization...")
    
    # Create a continuous color scale for birth cohorts
    min_birth_year = int(min(birthyears))
    max_birth_year = int(max(birthyears))
    plasma = cm.get_cmap('plasma')
    
    # Create figure
    fig = go.Figure()
    
    # Add aggregate trend line (always visible)
    agg_by_year = data.groupby('YEAR')['gender_gap'].mean().reset_index()
    fig.add_trace(
        go.Scatter(
            x=agg_by_year['YEAR'],
            y=agg_by_year['gender_gap'],
            mode='lines+markers',
            line=dict(color='black', width=3),
            marker=dict(size=6, color='black'),
            name='Aggregate Trend',
            showlegend=False,  # Remove from legend as requested
            hovertemplate='Year: %{x}<br>Gender Gap: %{y:.1f}%'
        )
    )
    
    # Group data by decade for better organization
    decade_groups = {}
    for birthyear in sorted(birthyears):
        decade = (birthyear // 10) * 10
        if decade not in decade_groups:
            decade_groups[decade] = []
        decade_groups[decade].append(birthyear)
    
    # Add traces for each birth cohort, grouped by decade
    for decade, cohort_years in decade_groups.items():
        for birthyear in cohort_years:
            cohort_data = data[data['BIRTHYEAR'] == birthyear]
            
            if len(cohort_data) > 1:  # Only add if we have enough points
                # Calculate color based on birth year
                color_val = (birthyear - min_birth_year) / (max_birth_year - min_birth_year)
                color = mcolors.rgb2hex(plasma(color_val))
                
                # Create trace name
                trace_name = f'{birthyear}'
                
                fig.add_trace(
                    go.Scatter(
                        x=cohort_data['YEAR'],
                        y=cohort_data['gender_gap'],
                        mode='lines+markers',
                        line=dict(color=color, width=1.5),
                        marker=dict(size=4, color=color),
                        opacity=0.7,
                        name=trace_name,
                        legendgroup=f'decade_{decade}',
                        showlegend=False,  # Hide from legend, will use color scale instead
                        hovertemplate=f'Year: %{{x}}<br>Gender Gap: %{{y:.1f}}%<br>Birth Year: {birthyear}'
                    )
                )
    
    # Create color scale legend for birth years
    # Create a range of birth years for the color scale
    birth_years = list(range(min_birth_year, max_birth_year + 1, 5))  # Every 5 years
    
    # Create color scale annotations
    colorscale_annotations = []
    for birthyear in birth_years:
        color_val = (birthyear - min_birth_year) / (max_birth_year - min_birth_year)
        color = mcolors.rgb2hex(plasma(color_val))
        
        colorscale_annotations.append(
            dict(
                x=1.05,  # Moved further to the right
                y=(birthyear - min_birth_year) / (max_birth_year - min_birth_year),
                xref='paper',
                yref='paper',
                text=str(birthyear),
                showarrow=False,
                font=dict(size=10),
                align='left'
            )
        )
    
    # Create color scale shapes
    colorscale_shapes = []
    for i in range(100):
        y_pos = i / 100
        color_val = y_pos
        color = mcolors.rgb2hex(plasma(color_val))
        
        colorscale_shapes.append(
            dict(
                type='rect',
                xref='paper',
                yref='paper',
                x0=1.03,  # Moved further to the right
                y0=y_pos,
                x1=1.04,  # Moved further to the right
                y1=y_pos + 0.01,
                fillcolor=color,
                line=dict(width=0)
            )
        )
    
    # Add title annotation for color scale
    colorscale_annotations.append(
        dict(
            x=1.05,  # Moved further to the right
            y=1.05,
            xref='paper',
            yref='paper',
            text='Birth Year',
            showarrow=False,
            font=dict(size=12, color='black'),
            align='left'
        )
    )
    
    # Add a dashed line at 0%
    zero_line = dict(
        type='line',
        xref='paper',
        yref='y',
        x0=0,
        y0=0,
        x1=1,
        y1=0,
        line=dict(
            color='gray',
            width=1,
            dash='dash'
        )
    )
    colorscale_shapes.append(zero_line)
    
    # Add a solid outline around the entire figure
    outline = dict(
        type='rect',
        xref='paper',
        yref='paper',
        x0=0,
        y0=0,
        x1=1,
        y1=1,
        line=dict(
            color='black',
            width=1
        ),
        fillcolor='rgba(0,0,0,0)'
    )
    colorscale_shapes.append(outline)
    
    # Add note at the bottom
    note_annotation = dict(
        x=0.5,
        y=-0.30,  # Moved even further down to avoid clashing with selectors
        xref='paper',
        yref='paper',
        text='Data from the Current Population Survey Outgoing Rotation Groups, non self−employed workers aged 25−55.<br>Solid black line shows aggregate period trend.',
        showarrow=False,
        font=dict(size=10, color='gray'),
        align='center'
    )
    colorscale_annotations.append(note_annotation)
    
    # Update layout
    fig.update_layout(
        title='Gender Pay Gap Over Time (1982-2020)',
        xaxis=dict(
            title='Year',
            gridcolor='lightgray',
            showgrid=True,
            tickformat='d',
            range=[min(years) - 1, max(years) + 1]
        ),
        yaxis=dict(
            title=dict(
                text='Gender Wage Gap (%)',
                standoff=15  # Add standoff to ensure consistent spacing
            ),
            gridcolor='lightgray',
            showgrid=True,
            ticksuffix='%',
            range=[-5, data['gender_gap'].max() * 1.1]  # Start at -5% to show the zero line
        ),
        plot_bgcolor='white',
        hovermode='closest',
        shapes=colorscale_shapes,
        annotations=colorscale_annotations,
        # Set consistent dimensions for all plots
        width=900,
        height=600,
        margin=dict(l=50, r=120, t=80, b=180),  # Increased bottom margin for note
        autosize=False  # Disable autosize to ensure consistent dimensions
    )
    
    # Add checkbox filter
    decades = sorted(decade_groups.keys())
    buttons = []
    
    # Add a button for each decade
    for decade in decades:
        visible_array = []
        for trace in fig.data:
            # Always show aggregate trend
            if trace.name == 'Aggregate Trend':
                visible_array.append(True)
            # Show traces from this decade
            elif hasattr(trace, 'legendgroup') and trace.legendgroup == f'decade_{decade}':
                visible_array.append(True)
            # Hide other traces
            else:
                visible_array.append(False)
        
        buttons.append(
            dict(
                label=f'Born {decade}s',
                method='update',
                args=[{'visible': visible_array}]
            )
        )
    
    # Add a button to show all cohorts
    buttons.append(
        dict(
            label='All Cohorts',
            method='update',
            args=[{'visible': [True] * len(fig.data)}]
        )
    )
    
    # Create a single row of buttons that spans the width of the figure
    button_row = dict(
        type='buttons',
        direction='right',
        buttons=buttons,
        pad={"r": 10, "t": 10},
        showactive=True,
        x=0.5,
        xanchor='center',
        y=-0.10,  # Moved further down
        yanchor='top',
        font=dict(size=14),
        bgcolor='rgba(240, 240, 240, 0.8)',
        bordercolor='rgba(0, 0, 0, 0.3)',
        borderwidth=1
    )
    
    # Add the buttons to the layout
    fig.update_layout(
        updatemenus=[button_row]
    )
    
    return fig

# Create cohort visualization (by birth year)
def create_cohort_visualization(data, years, birthyears):
    print("Creating cohort visualization...")
    
    # Create a continuous color scale for birth years
    min_birth_year = int(min(birthyears))
    max_birth_year = int(max(birthyears))
    plasma = cm.get_cmap('plasma')
    
    # Create figure
    fig = go.Figure()
    
    # Group data by year for filtering
    year_groups = {}
    for year in sorted(years):
        year_groups[year] = data[data['YEAR'] == year]
    
    # Add traces for each birth year
    for birthyear in sorted(birthyears):
        # Get data for this birth year across all years
        birthyear_data = data[data['BIRTHYEAR'] == birthyear].sort_values('YEAR')
        
        if len(birthyear_data) > 1:  # Only add if we have enough points
            # Calculate color based on birth year
            color_val = (birthyear - min_birth_year) / (max_birth_year - min_birth_year)
            color = mcolors.rgb2hex(plasma(color_val))
            
            # Create trace name
            trace_name = f'{birthyear}'
            
            # Add to figure
            fig.add_trace(
                go.Scatter(
                    x=birthyear_data['BIRTHYEAR'],
                    y=birthyear_data['gender_gap'],
                    mode='lines+markers',
                    line=dict(color=color, width=1.5),
                    marker=dict(size=4, color=color),
                    name=trace_name,
                    legendgroup=f'year_{birthyear_data["YEAR"].iloc[0]}',  # Group by first year for filtering
                    showlegend=False,  # Hide from legend
                    hovertemplate=f'Birth Year: %{{x}}<br>Gender Gap: %{{y:.1f}}%<br>Birth Year: {birthyear}'
                )
            )
    
    # Create color scale legend for birth years
    # Create a range of birth years for the color scale
    birth_years = list(range(min_birth_year, max_birth_year + 1, 5))  # Every 5 years
    
    # Create color scale annotations
    colorscale_annotations = []
    for birthyear in birth_years:
        color_val = (birthyear - min_birth_year) / (max_birth_year - min_birth_year)
        color = mcolors.rgb2hex(plasma(color_val))
        
        colorscale_annotations.append(
            dict(
                x=1.05,  # Moved further to the right
                y=(birthyear - min_birth_year) / (max_birth_year - min_birth_year),
                xref='paper',
                yref='paper',
                text=str(birthyear),
                showarrow=False,
                font=dict(size=10),
                align='left'
            )
        )
    
    # Create color scale shapes
    colorscale_shapes = []
    for i in range(100):
        y_pos = i / 100
        color_val = y_pos
        color = mcolors.rgb2hex(plasma(color_val))
        
        colorscale_shapes.append(
            dict(
                type='rect',
                xref='paper',
                yref='paper',
                x0=1.03,  # Moved further to the right
                y0=y_pos,
                x1=1.04,  # Moved further to the right
                y1=y_pos + 0.01,
                fillcolor=color,
                line=dict(width=0)
            )
        )
    
    # Add title annotation for color scale
    colorscale_annotations.append(
        dict(
            x=1.05,  # Moved further to the right
            y=1.05,
            xref='paper',
            yref='paper',
            text='Birth Year',  # Changed back to Birth Year
            showarrow=False,
            font=dict(size=12, color='black'),
            align='left'
        )
    )
    
    # Add a dashed line at 0%
    zero_line = dict(
        type='line',
        xref='paper',
        yref='y',
        x0=0,
        y0=0,
        x1=1,
        y1=0,
        line=dict(
            color='gray',
            width=1,
            dash='dash'
        )
    )
    colorscale_shapes.append(zero_line)
    
    # Add a solid outline around the entire figure
    outline = dict(
        type='rect',
        xref='paper',
        yref='paper',
        x0=0,
        y0=0,
        x1=1,
        y1=1,
        line=dict(
            color='black',
            width=1
        ),
        fillcolor='rgba(0,0,0,0)'
    )
    colorscale_shapes.append(outline)
    
    # Add note at the bottom
    note_annotation = dict(
        x=0.5,
        y=-0.25,  # Moved further down
        xref='paper',
        yref='paper',
        text='Data from the Current Population Survey Outgoing Rotation Groups, non self−employed workers aged 25−55.',
        showarrow=False,
        font=dict(size=10, color='gray'),
        align='center'
    )
    colorscale_annotations.append(note_annotation)
    
    # Update layout
    fig.update_layout(
        title='Gender Pay Gap by Birth Cohort',
        xaxis=dict(
            title='Birth Year',
            gridcolor='lightgray',
            showgrid=True,
            tickformat='d',
            range=[min(birthyears) - 2, max(birthyears) + 2]
        ),
        yaxis=dict(
            title=dict(
                text='Gender Wage Gap (%)',
                standoff=15  # Add standoff to ensure consistent spacing
            ),
            gridcolor='lightgray',
            showgrid=True,
            ticksuffix='%',
            range=[-5, data['gender_gap'].max() * 1.1]  # Start at -5% to show the zero line
        ),
        plot_bgcolor='white',
        hovermode='closest',
        shapes=colorscale_shapes,
        annotations=colorscale_annotations,
        # Set consistent dimensions for all plots
        width=900,
        height=600,
        margin=dict(l=50, r=120, t=80, b=180),  # Increased bottom margin for note
        autosize=False  # Disable autosize to ensure consistent dimensions
    )
    
    # Group years by decade for better organization
    decade_groups = {}
    for year in sorted(years):
        decade = (year // 10) * 10
        if decade not in decade_groups:
            decade_groups[decade] = []
        decade_groups[decade].append(year)
    
    buttons = []
    
    # Add a button for each DECADE (not individual years)
    for decade, decade_years in decade_groups.items():
        visible_array = []
        
        # For each trace, check if it should be visible for this decade
        for trace in fig.data:
            # Get the birth year from the trace name
            trace_birth_year = int(trace.name) if trace.name.isdigit() else None
            
            if trace_birth_year is not None:
                # Check if this birth year exists in the data for any year in this decade
                is_visible = False
                for year in decade_years:
                    year_data = year_groups[year]
                    if trace_birth_year in year_data['BIRTHYEAR'].values:
                        is_visible = True
                        break
                
                visible_array.append(is_visible)
            else:
                visible_array.append(False)
        
        buttons.append(
            dict(
                label=f'{decade}s',
                method='update',
                args=[{'visible': visible_array}]
            )
        )
    
    # Add a button to show all years
    buttons.append(
        dict(
            label='All Years',
            method='update',
            args=[{'visible': [True] * len(fig.data)}]
        )
    )
    
    # Create a single row of buttons that spans the width of the figure
    button_row = dict(
        type='buttons',
        direction='right',
        buttons=buttons,
        pad={"r": 10, "t": 10},
        showactive=True,
        x=0.5,
        xanchor='center',
        y=-0.10,  # Moved further down
        yanchor='top',
        font=dict(size=14),
        bgcolor='rgba(240, 240, 240, 0.8)',
        bordercolor='rgba(0, 0, 0, 0.3)',
        borderwidth=1
    )
    
    # Add the buttons to the layout
    fig.update_layout(
        updatemenus=[button_row]
    )
    
    return fig

# Create age visualization
def create_age_visualization(data, birthyears):
    print("Creating age visualization...")
    
    # Create a continuous color scale for birth cohorts
    min_birth_year = int(min(birthyears))
    max_birth_year = int(max(birthyears))
    plasma = cm.get_cmap('plasma')
    
    # Create figure
    fig = go.Figure()
    
    # Group data by decade for better organization
    decade_groups = {}
    for birthyear in sorted(birthyears):
        decade = (birthyear // 10) * 10
        if decade not in decade_groups:
            decade_groups[decade] = []
        decade_groups[decade].append(birthyear)
    
    # Add traces for each birth cohort, grouped by decade
    for decade, cohort_years in decade_groups.items():
        for birthyear in cohort_years:
            cohort_data = data[data['BIRTHYEAR'] == birthyear]
            
            if len(cohort_data) > 1:  # Only add if we have enough points
                # Calculate color based on birth year
                color_val = (birthyear - min_birth_year) / (max_birth_year - min_birth_year)
                color = mcolors.rgb2hex(plasma(color_val))
                
                # Create trace name
                trace_name = f'{birthyear}'
                
                fig.add_trace(
                    go.Scatter(
                        x=cohort_data['AGE'],
                        y=cohort_data['gender_gap'],
                        mode='lines+markers',
                        line=dict(color=color, width=1.5),
                        marker=dict(size=4, color=color),
                        opacity=0.7,
                        name=trace_name,
                        legendgroup=f'decade_{decade}',
                        showlegend=False,  # Hide from legend, will use color scale instead
                        hovertemplate=f'Age: %{{x}}<br>Gender Gap: %{{y:.1f}}%<br>Birth Year: {birthyear}'
                    )
                )
    
    # Create color scale legend
    colorscale_shapes, colorscale_annotations = create_colorscale_legend(min_birth_year, max_birth_year)
    
    # Add title annotation for color scale
    colorscale_annotations.append(
        dict(
            x=1.05,  # Moved further to the right
            y=1.05,
            xref='paper',
            yref='paper',
            text='Birth Year',
            showarrow=False,
            font=dict(size=12, color='black'),
            align='left'
        )
    )
    
    # Add a dashed line at 0%
    zero_line = dict(
        type='line',
        xref='paper',
        yref='y',
        x0=0,
        y0=0,
        x1=1,
        y1=0,
        line=dict(
            color='gray',
            width=1,
            dash='dash'
        )
    )
    colorscale_shapes.append(zero_line)
    
    # Add a solid outline around the entire figure
    outline = dict(
        type='rect',
        xref='paper',
        yref='paper',
        x0=0,
        y0=0,
        x1=1,
        y1=1,
        line=dict(
            color='black',
            width=1
        ),
        fillcolor='rgba(0,0,0,0)'
    )
    colorscale_shapes.append(outline)
    
    # Add note at the bottom
    note_annotation = dict(
        x=0.5,
        y=-0.25,  # Moved further down
        xref='paper',
        yref='paper',
        text='Data from the Current Population Survey Outgoing Rotation Groups, non self−employed workers aged 25−55.',
        showarrow=False,
        font=dict(size=10, color='gray'),
        align='center'
    )
    colorscale_annotations.append(note_annotation)
    
    # Update layout
    fig.update_layout(
        title='Gender Pay Gap by Age',
        xaxis=dict(
            title='Age',
            gridcolor='lightgray',
            showgrid=True,
            tickformat='d',
            range=[25, 55]
        ),
        yaxis=dict(
            title=dict(
                text='Gender Wage Gap (%)',
                standoff=15  # Add standoff to ensure consistent spacing
            ),
            gridcolor='lightgray',
            showgrid=True,
            ticksuffix='%',
            range=[-5, data['gender_gap'].max() * 1.1]  # Start at -5% to show the zero line
        ),
        plot_bgcolor='white',
        hovermode='closest',
        shapes=colorscale_shapes,
        annotations=colorscale_annotations,
        # Set consistent dimensions for all plots
        width=900,
        height=600,
        margin=dict(l=50, r=120, t=80, b=180),  # Increased bottom margin for note
        autosize=False  # Disable autosize to ensure consistent dimensions
    )
    
    # Add checkbox filter
    decades = sorted(decade_groups.keys())
    buttons = []
    
    # Add a button for each decade
    for decade in decades:
        visible_array = []
        for trace in fig.data:
            # Show traces from this decade
            if hasattr(trace, 'legendgroup') and trace.legendgroup == f'decade_{decade}':
                visible_array.append(True)
            # Hide other traces
            else:
                visible_array.append(False)
        
        buttons.append(
            dict(
                label=f'Born {decade}s',
                method='update',
                args=[{'visible': visible_array}]
            )
        )
    
    # Add a button to show all cohorts
    buttons.append(
        dict(
            label='All Cohorts',
            method='update',
            args=[{'visible': [True] * len(fig.data)}]
        )
    )
    
    # Create a single row of buttons that spans the width of the figure
    button_row = dict(
        type='buttons',
        direction='right',
        buttons=buttons,
        pad={"r": 10, "t": 10},
        showactive=True,
        x=0.5,
        xanchor='center',
        y=-0.10,  # Moved further down
        yanchor='top',
        font=dict(size=14),
        bgcolor='rgba(240, 240, 240, 0.8)',
        bordercolor='rgba(0, 0, 0, 0.3)',
        borderwidth=1
    )
    
    # Add the buttons to the layout
    fig.update_layout(
        updatemenus=[button_row]
    )
    
    return fig

# Create main HTML file
def create_main_html():
    print("Creating main HTML file...")
    with open('gender_gap_visualization_final_v3.html', 'w') as f:
        f.write('''
        <!DOCTYPE html>
        <html>
        <head>
            <title>Gender Pay Gap Visualization</title>
            <style>
                body, html {
                    font-family: Arial, sans-serif;
                    margin: 0;
                    padding: 0;
                    height: 100%;
                    width: 100%;
                    overflow: hidden;
                }
                .container {
                    width: 100%;
                    height: 100vh;
                    display: flex;
                    flex-direction: column;
                    align-items: center;  /* Center content horizontally */
                }
                .header {
                    text-align: center;
                    padding: 10px 0;
                    width: 100%;
                }
                h1 {
                    margin: 0;
                    color: #333;
                    font-size: 28px;
                }
                h2 {
                    margin: 5px 0 0 0;
                    color: #666;
                    font-weight: normal;
                    font-size: 18px;
                }
                .view-selector {
                    display: flex;
                    justify-content: center;
                    padding: 10px 0;
                    gap: 10px;
                    width: 100%;
                }
                .view-button {
                    padding: 8px 16px;
                    background-color: #f0f0f0;
                    border: 1px solid #ddd;
                    border-radius: 4px;
                    cursor: pointer;
                    font-size: 14px;
                    transition: all 0.3s ease;
                }
                .view-button:hover {
                    background-color: #e0e0e0;
                }
                .view-button.active {
                    background-color: #007bff;
                    color: white;
                    border-color: #0056b3;
                }
                .viz-container {
                    display: none;
                    flex: 1;
                    width: 100%;
                    text-align: center;  /* Center iframe */
                }
                .viz-container.active {
                    display: block;
                }
                iframe {
                    width: 100%;
                    height: 750px;  /* Increased height to ensure buttons are visible */
                    border: none;
                    max-width: 1000px;  /* Limit width to ensure centering */
                    margin: 0 auto;     /* Center the iframe */
                    overflow: auto;     /* Add scrollbars if needed */
                }
            </style>
        </head>
        <body>
            <div class="container">
                <div class="header">
                    <h1>The Evolution of the Gender Pay Gap (1982-2020)</h1>
                    <h2>An Interactive Visualization of Cohort and Period Trends</h2>
                </div>
                
                <div class="view-selector">
                    <button class="view-button active" data-view="period">View by Period (Year)</button>
                    <button class="view-button" data-view="cohort">View by Birth Cohort</button>
                    <button class="view-button" data-view="age">View by Age</button>
                </div>
                
                <div class="viz-container active" id="period-view">
                    <iframe src="gender_gap_by_period.html"></iframe>
                </div>
                
                <div class="viz-container" id="cohort-view">
                    <iframe src="gender_gap_by_cohort.html"></iframe>
                </div>
                
                <div class="viz-container" id="age-view">
                    <iframe src="gender_gap_by_age.html"></iframe>
                </div>
            </div>
            
            <script>
                // Toggle between views
                const viewButtons = document.querySelectorAll('.view-button');
                const vizContainers = document.querySelectorAll('.viz-container');
                
                viewButtons.forEach(button => {
                    button.addEventListener('click', () => {
                        // Remove active class from all buttons and containers
                        viewButtons.forEach(btn => btn.classList.remove('active'));
                        vizContainers.forEach(container => container.classList.remove('active'));
                        
                        // Add active class to clicked button and corresponding container
                        button.classList.add('active');
                        const viewType = button.getAttribute('data-view');
                        document.getElementById(viewType + '-view').classList.add('active');
                    });
                });
            </script>
        </body>
        </html>
        ''')

# Main function
def main():
    print("Starting visualization generation...")
    
    # Load data
    df = load_data()
    
    # Process data
    data = process_data(df)
    
    # Get unique years and birth years
    years = sorted(data['YEAR'].unique())
    birthyears = sorted(data['BIRTHYEAR'].unique())
    
    # Create visualizations
    fig_period = create_period_visualization(data, years, birthyears)
    fig_cohort = create_cohort_visualization(data, years, birthyears)
    fig_age = create_age_visualization(data, birthyears)
    
    # Save visualizations as HTML files
    print("Saving HTML files...")
    fig_period.write_html('gender_gap_by_period.html', include_plotlyjs='cdn')
    print("Saved period visualization")
    fig_cohort.write_html('gender_gap_by_cohort.html', include_plotlyjs='cdn')
    print("Saved cohort visualization")
    fig_age.write_html('gender_gap_by_age.html', include_plotlyjs='cdn')
    print("Saved age visualization")
    
    # Create main HTML file
    create_main_html()
    print("Main HTML file created")
    
    # Print file paths for debugging
    print(f"Files saved to: {os.getcwd()}")
    print(f"Period file: {os.path.abspath('gender_gap_by_period.html')}")
    print(f"Cohort file: {os.path.abspath('gender_gap_by_cohort.html')}")
    print(f"Age file: {os.path.abspath('gender_gap_by_age.html')}")
    print(f"Main file: {os.path.abspath('gender_gap_visualization_final_v3.html')}")

if __name__ == "__main__":
    try:
        main()
        print("Script completed successfully!")
    except Exception as e:
        import traceback
        print(f"Error: {e}")
        print(traceback.format_exc())
