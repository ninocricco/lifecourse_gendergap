import pandas as pd
import plotly.graph_objects as go
import plotly.io as pio
import os

import matplotlib.cm as cm
import matplotlib.colors as mcolors

import warnings
warnings.filterwarnings('ignore')


#############################
# 1) Load and process data
#############################

def load_data():
    print("Loading pre-summarized data from CSV file...")
    file_path = "clean_data/means_age_year_sex_org.csv"
    df = pd.read_csv(file_path)
    
    # Rename columns to match expected format
    df.columns = ['AGE', 'BIRTHYEAR', 'YEAR', 'FEMALE', 'MEAN_OUTCOME', 'n']
    
    # Convert FEMALE column to match expected format (0 for Men, 1 for Women)
    df['FEMALE'] = df['FEMALE'].map({'Men': 0, 'Women': 1})
    
    print(f"Data loaded with {len(df)} records")
    return df


def process_data(df):
    print("Processing data...")
    means_wide = df.pivot_table(
        index=['YEAR', 'BIRTHYEAR'], 
        columns='FEMALE', 
        values='MEAN_OUTCOME'
    ).reset_index()
    
    means_wide.columns.name = None
    means_wide = means_wide.rename(columns={0: 'Men', 1: 'Women'})
    
    means_wide['ratio'] = means_wide['Women'] / means_wide['Men']
    means_wide['gender_gap'] = (1 - means_wide['ratio']) * 100
    means_wide['AGE'] = means_wide['YEAR'] - means_wide['BIRTHYEAR']
    
    print(f"Data processed: {len(means_wide)} year-cohort combinations")
    return means_wide


###########################
# 2) Helper for color scale
###########################

def create_colorscale_legend(min_birth_year, max_birth_year):
    plasma = cm.get_cmap('plasma')
    birth_years = list(range(min_birth_year, max_birth_year + 1, 5))

    annotations = []
    for by in birth_years:
        if max_birth_year > min_birth_year:
            color_val = (by - min_birth_year) / (max_birth_year - min_birth_year)
        else:
            color_val = 0
        color = mcolors.rgb2hex(plasma(color_val))
        annotations.append(
            dict(
                x=1.08,
                y=color_val,
                xref='paper',
                yref='paper',
                text=str(by),
                showarrow=False,
                font=dict(size=10),
                align='left'
            )
        )

    shapes = []
    for i in range(100):
        y_pos = i / 100
        color_val = y_pos
        color = mcolors.rgb2hex(plasma(color_val))
        shapes.append(
            dict(
                type='rect',
                xref='paper',
                yref='paper',
                x0=1.03,
                y0=y_pos,
                x1=1.04,
                y1=y_pos + 0.01,
                fillcolor=color,
                line=dict(width=0)
            )
        )
    return shapes, annotations


############################
# 3) Three figure functions
############################

def create_period_visualization(data, years, birthyears):
    """
    Panel 1: Period. X=Year, Y=GenderGap, color-coded by BirthYear.
    Hover: Year, Birth Cohort, Gap.
    """
    min_by = int(min(birthyears))
    max_by = int(max(birthyears))
    plasma = cm.get_cmap('plasma')
    
    fig = go.Figure()
    
    # Add an aggregate trend
    agg_by_year = data.groupby('YEAR')['gender_gap'].mean().reset_index()
    fig.add_trace(
        go.Scatter(
            x=agg_by_year['YEAR'],
            y=agg_by_year['gender_gap'],
            mode='lines+markers',
            line=dict(color='black', width=2),
            marker=dict(size=6, color='black'),
            name='Aggregate Trend',
            showlegend=False,
            hovertemplate='Year: %{x}<br>Gap: %{y:.1f}%'
        )
    )
    
    # Group data by decade
    decade_groups = {}
    for by in sorted(birthyears):
        decade = (by // 10) * 10
        decade_groups.setdefault(decade, []).append(by)
    
    # Add a line for each birth cohort
    for decade, cohort_list in decade_groups.items():
        for by in cohort_list:
            subset = data[data['BIRTHYEAR'] == by]
            if len(subset) > 1:
                color_val = (by - min_by)/(max_by - min_by) if (max_by>min_by) else 0
                color = mcolors.rgb2hex(plasma(color_val))
                fig.add_trace(
                    go.Scatter(
                        x=subset['YEAR'],
                        y=subset['gender_gap'],
                        mode='lines+markers',
                        line=dict(color=color, width=1.5),
                        marker=dict(size=4, color=color),
                        opacity=0.7,
                        name=str(by),
                        legendgroup=f'decade_{decade}',
                        showlegend=False,
                        hovertemplate=(
                            f"Year: %{{x}}<br>"
                            f"Birth Cohort: {by}<br>"
                            f"Gap: %{{y:.1f}}%"
                        )
                    )
                )
    
    shapes, annotations = create_colorscale_legend(min_by, max_by)
    annotations.append(
        dict(
            x=1.08,
            y=1.05,
            xref='paper',
            yref='paper',
            text='Birth Year',
            showarrow=False,
            font=dict(size=12, color='black'),
            align='left'
        )
    )
    
    # A dashed line at 0%
    zero_line = dict(
        type='line',
        xref='paper',
        yref='y',
        x0=0,
        y0=0,
        x1=1,
        y1=0,
        line=dict(color='gray', width=1, dash='dash')
    )
    shapes.append(zero_line)
    
    # Outline
    outline = dict(
        type='rect',
        xref='paper',
        yref='paper',
        x0=0, y0=0,
        x1=1, y1=1,
        line=dict(color='black', width=1),
        fillcolor='rgba(0,0,0,0)'
    )
    shapes.append(outline)
    
    # Layout
    fig.update_layout(
        paper_bgcolor='white',
        plot_bgcolor='white',
        xaxis=dict(
            title='Year',
            range=[min(years)-1, max(years)+1],
            gridcolor='lightgray'
        ),
        yaxis=dict(
            title='Gender Wage Gap (%)',
            range=[-5, data['gender_gap'].max()*1.1],
            gridcolor='lightgray'
        ),
        # Make the figure bigger, with enough bottom margin for the buttons
        width=950,
        height=650,
        margin=dict(l=100, r=80, t=40, b=220),
        shapes=shapes,
        annotations=annotations,
        legend=dict(
            x=.9, y=1,
            xanchor='left',
            yanchor='top',
            borderwidth=0
        )
    )
    
    # Create updatemenu for decades
    sorted_decades = sorted(decade_groups.keys())
    buttons = []
    for decade in sorted_decades:
        visible_array = []
        for trace in fig.data:
            if trace.name == 'Aggregate Trend':
                visible_array.append(True)
            else:
                visible_array.append(trace.legendgroup == f'decade_{decade}')
        
        buttons.append(
            dict(
                label=f'{decade}s',
                method='update',
                args=[{'visible': visible_array}]
            )
        )
    # "All Cohorts"
    buttons.append(
        dict(
            label='All Cohorts',
            method='update',
            args=[{'visible': [True]*len(fig.data)}]
        )
    )
    
    updatemenu = dict(
        type='buttons',
        buttons=buttons,
        direction='right',
        x=0.5, xanchor='center',
        # Move the menu to about 25% below the chart area
        y=-0.1, yanchor='top',
        pad={"r": 10, "t": 10},
        showactive=True
    )
    fig.update_layout(updatemenus=[updatemenu])
    return fig


def create_cohort_visualization(data, years, birthyears):
    """
    Panel 2: Cohort. X=Year, Y=Gap, color-coded by birth year, filtered by calendar-year decade.
    Hover: Year, Birth Cohort, Gap.
    """
    min_by, max_by = int(min(birthyears)), int(max(birthyears))
    plasma = cm.get_cmap('plasma')
    
    fig = go.Figure()
    
    # Aggregate line
    agg_by_year = data.groupby('YEAR')['gender_gap'].mean().reset_index()
    fig.add_trace(
        go.Scatter(
            x=agg_by_year['YEAR'],
            y=agg_by_year['gender_gap'],
            mode='lines+markers',
            line=dict(color='black', width=2),
            marker=dict(size=6, color='black'),
            name='Aggregate Trend',
            showlegend=False,
            hovertemplate='Year: %{x}<br>Gap: %{y:.1f}%'
        )
    )
    
    for by in sorted(birthyears):
        subset = data[data['BIRTHYEAR'] == by].sort_values('YEAR')
        if len(subset) > 1:
            color_val = (by - min_by)/(max_by - min_by) if (max_by>min_by) else 0
            color = mcolors.rgb2hex(plasma(color_val))
            fig.add_trace(
                go.Scatter(
                    x=subset['YEAR'],
                    y=subset['gender_gap'],
                    mode='lines+markers',
                    opacity=0.7,
                    line=dict(color=color, width=1.5),
                    marker=dict(size=4, color=color),
                    name=str(by),
                    legendgroup=f'decade_{(by//10)*10}',
                    showlegend=False,
                    hovertemplate=(
                        f"Year: %{{x}}<br>"
                        f"Birth Cohort: {by}<br>"
                        f"Gap: %{{y:.1f}}%"
                    )
                )
            )
    
    shapes, annotations = create_colorscale_legend(min_by, max_by)
    annotations.append(
        dict(
            x=1.08,
            y=1.05,
            xref='paper',
            yref='paper',
            text='Birth Year',
            showarrow=False,
            font=dict(size=12, color='black'),
            align='left'
        )
    )
    
    zero_line = dict(
        type='line',
        xref='paper',
        yref='y',
        x0=0, y0=0,
        x1=1, y1=0,
        line=dict(color='gray', width=1, dash='dash')
    )
    shapes.append(zero_line)
    
    outline = dict(
        type='rect',
        xref='paper',
        yref='paper',
        x0=0, y0=0,
        x1=1, y1=1,
        line=dict(color='black', width=1),
        fillcolor='rgba(0,0,0,0)'
    )
    shapes.append(outline)
    
    fig.update_layout(
        paper_bgcolor='white',
        plot_bgcolor='white',
        xaxis=dict(
            title='Year',
            range=[min(years)-1, max(years)+1],
            gridcolor='lightgray'
        ),
        yaxis=dict(
            title='Gender Wage Gap (%)',
            range=[-5, data['gender_gap'].max()*1.1],
            gridcolor='lightgray'
        ),
        width=950,
        height=650,
        margin=dict(l=100, r=80, t=40, b=220),
        shapes=shapes,
        annotations=annotations,
        legend=dict(
            x=.9, y=1,
            xanchor='left',
            yanchor='top',
            borderwidth=0
        )
    )
    
    # Group calendar years by decade
    decade_groups = {}
    for yr in sorted(years):
        dec = (yr // 10)*10
        decade_groups.setdefault(dec, []).append(yr)
    
    buttons = []
    for decade, dec_years in sorted(decade_groups.items()):
        visible_array = []
        for trace in fig.data:
            if trace.name == 'Aggregate Trend':
                visible_array.append(True)
            else:
                try:
                    trace_by = int(trace.name)
                    # Show if that birth year appears in any year in dec_years
                    is_visible = any(
                        trace_by in data[data['YEAR'] == y]['BIRTHYEAR'].values
                        for y in dec_years
                    )
                    visible_array.append(is_visible)
                except:
                    visible_array.append(False)
        
        buttons.append(
            dict(
                label=f'{decade}s',
                method='update',
                args=[{'visible': visible_array}]
            )
        )
    # "All Years"
    buttons.append(
        dict(
            label='All Years',
            method='update',
            args=[{'visible': [True]*len(fig.data)}]
        )
    )
    
    updatemenu = dict(
        type='buttons',
        buttons=buttons,
        direction='right',
        x=0.5, xanchor='center',
        y=-0.1, yanchor='top',
        pad={"r": 10, "t": 10},
        showactive=True
    )
    fig.update_layout(updatemenus=[updatemenu])
    return fig


def create_age_visualization(data, birthyears):
    """
    Panel 3: Age. X=Age, Y=Gap, color-coded by birth year.
    Hover: Age, Birth Cohort, Gap
    """
    min_by, max_by = int(min(birthyears)), int(max(birthyears))
    plasma = cm.get_cmap('plasma')
    
    fig = go.Figure()
    
    # Group birthyears by decade
    decade_groups = {}
    for by in sorted(birthyears):
        dec = (by // 10)*10
        decade_groups.setdefault(dec, []).append(by)
    
    for decade, cohort_list in decade_groups.items():
        for by in cohort_list:
            subset = data[data['BIRTHYEAR'] == by]
            if len(subset) > 1:
                color_val = (by - min_by)/(max_by - min_by) if (max_by>min_by) else 0
                color = mcolors.rgb2hex(plasma(color_val))
                fig.add_trace(
                    go.Scatter(
                        x=subset['AGE'],
                        y=subset['gender_gap'],
                        mode='lines+markers',
                        line=dict(color=color, width=1.5),
                        marker=dict(size=4, color=color),
                        opacity=0.7,
                        name=str(by),
                        legendgroup=f'decade_{decade}',
                        showlegend=False,
                        hovertemplate=(
                            f"Age: %{{x}}<br>"
                            f"Birth Cohort: {by}<br>"
                            f"Gap: %{{y:.1f}}%"
                        )
                    )
                )
    
    shapes, annotations = create_colorscale_legend(min_by, max_by)
    annotations.append(
        dict(
            x=1.08,
            y=1.05,
            xref='paper',
            yref='paper',
            text='Birth Year',
            showarrow=False,
            font=dict(size=12, color='black'),
            align='left'
        )
    )
    
    zero_line = dict(
        type='line',
        xref='paper',
        yref='y',
        x0=0, y0=0,
        x1=1, y1=0,
        line=dict(color='gray', width=1, dash='dash')
    )
    shapes.append(zero_line)
    
    outline = dict(
        type='rect',
        xref='paper',
        yref='paper',
        x0=0, y0=0,
        x1=1, y1=1,
        line=dict(color='black', width=1),
        fillcolor='rgba(0,0,0,0)'
    )
    shapes.append(outline)
    
    fig.update_layout(
        paper_bgcolor='white',
        plot_bgcolor='white',
        xaxis=dict(
            title='Age',
            range=[25,55],
            gridcolor='lightgray'
        ),
        yaxis=dict(
            title='Gender Wage Gap (%)',
            range=[-5, data['gender_gap'].max()*1.1],
            gridcolor='lightgray'
        ),
        width=950,
        height=650,
        margin=dict(l=100, r=80, t=40, b=220),
        shapes=shapes,
        annotations=annotations,
        legend=dict(
            x=.9, y=1,
            xanchor='left',
            yanchor='top',
            borderwidth=0
        )
    )
    
    # updatemenu by decade
    sorted_decades = sorted(decade_groups.keys())
    buttons = []
    for decade in sorted_decades:
        visible_array = []
        for trace in fig.data:
            visible_array.append(trace.legendgroup == f'decade_{decade}')
        
        buttons.append(
            dict(
                label=f'{decade}s',
                method='update',
                args=[{'visible': visible_array}]
            )
        )
    # all cohorts
    buttons.append(
        dict(
            label='All Cohorts',
            method='update',
            args=[{'visible': [True]*len(fig.data)}]
        )
    )
    
    updatemenu = dict(
        type='buttons',
        buttons=buttons,
        direction='right',
        x=0.5, xanchor='center',
        y=-0.1, yanchor='top',
        pad={"r": 10, "t": 10},
        showactive=True
    )
    fig.update_layout(updatemenus=[updatemenu])
    return fig


##############################
# 4) Main: create a single HTML
##############################

def main():
    print("Starting script...")

    # 1) load & process data
    df = load_data()
    data = process_data(df)

    years = sorted(data['YEAR'].unique())
    birthyears = sorted(data['BIRTHYEAR'].unique())

    # 2) Create the three figures
    fig_period = create_period_visualization(data, years, birthyears)
    fig_cohort = create_cohort_visualization(data, years, birthyears)
    fig_age = create_age_visualization(data, birthyears)

    # 3) Convert each figure to partial HTML
    import plotly.io as pio
    html_period = (
        "<!-- PERIOD FIGURE -->\n" +
        pio.to_html(fig_period, include_plotlyjs=False, full_html=False, div_id="period_div")
    )
    html_cohort = (
        "<!-- COHORT FIGURE -->\n" +
        pio.to_html(fig_cohort, include_plotlyjs=False, full_html=False, div_id="cohort_div")
    )
    html_age = (
        "<!-- AGE FIGURE -->\n" +
        pio.to_html(fig_age, include_plotlyjs=False, full_html=False, div_id="age_div")
    )

    # 4) Build one "all_in_one.html" page
    html_output = f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8"/>
    <title>Gender Pay Gap Age Period Cohort Visualization</title>
    <!-- Load Plotly once from CDN -->
    <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    <style>
      body {{
        font-family: Arial, sans-serif;
        margin: 0; padding: 0;
      }}
      /* Make the main title & sub-title smaller */
      h1 {{
        text-align: center;
        font-size: 20px; /* smaller than default ~2em */
        margin: 5px 0;
      }}
      h2 {{
        text-align: center;
        font-size: 14px;
        margin: 5px 0;
      }}
      .caption {{
        text-align: center;
        font-size: 10px;
        color: #777;
        margin-bottom: 10px;
        font-style: italic;
      }}
      .view-selector {{
        text-align: center;
        margin-bottom: 10px;
      }}
      .view-button {{
        width: 120px;
        margin: 0 5px;
        padding: 8px;
        background-color: #f0f0f0;
        border: 1px solid #ddd;
        border-radius: 4px;
        cursor: pointer;
      }}
      .view-button:hover {{
        background-color: #e0e0e0;
      }}
      .active-button {{
        background-color: #007bff;
        color: white;
        border-color: #0056b3;
      }}
      /* each figure is in a .viz-container <div> */
      .viz-container {{
        display: none;  /* hidden by default */
        width: 1100px;  /* slightly wider container */
        margin: 0 auto;
      }}
      .viz-container.active {{
        display: block; /* shown if 'active' class is present */
      }}
    </style>
</head>
<body>

  <h1>The Gender Pay Gap in the United States, 1982-2023</h1>
  <h2>An Interactive Visualization of Age, Period, and Cohort Trends</h2>
  <p class="caption">Data on hourly wages from the Current Population Survey Outgoing Rotation Groups, 
     non self−employed workers aged 25−55. Created by Nino Cricco.</p>

  <div class="view-selector">
    <button class="view-button active-button" data-target="period_div">Birth Cohort</button>
    <button class="view-button" data-target="cohort_div">Period</button>
    <button class="view-button" data-target="age_div">Age</button>
  </div>

  <!-- The 3 figure divs, only one is "active" at a time -->
  <div id="period_div" class="viz-container active">
    {html_period}
  </div>

  <div id="cohort_div" class="viz-container">
    {html_cohort}
  </div>

  <div id="age_div" class="viz-container">
    {html_age}
  </div>

  <script>
    // Switch .active classes when user clicks a button
    const buttons = document.querySelectorAll('.view-button');
    const containers = document.querySelectorAll('.viz-container');

    buttons.forEach(btn => {{
      btn.addEventListener('click', () => {{
        // remove active from all buttons
        buttons.forEach(b => b.classList.remove('active-button'));
        // remove active from all containers
        containers.forEach(c => c.classList.remove('active'));

        // add active to the clicked button
        btn.classList.add('active-button');
        // show the targeted container
        const targetId = btn.getAttribute('data-target');
        document.getElementById(targetId).classList.add('active');
      }});
    }});
  </script>

</body>
</html>"""

    out_filename = "gendergap_apc_interactive.html"
    with open(out_filename, "w", encoding="utf-8") as f:
        f.write(html_output)

    print(f"Created: {out_filename}")
    print("Done! Open 'gendergap_apc_interactive.html' in your browser to see all three figures without iframes.")


if __name__ == "__main__":
    main()
