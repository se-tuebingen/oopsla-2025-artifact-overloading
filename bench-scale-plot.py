#!/usr/bin/env nix-shell
#! nix-shell -i python3 -p python3 python3Packages.matplotlib python3Packages.pandas python3Packages.numpy

import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import argparse
import sys
from pathlib import Path

def parse_csv(csv_file):
    """Parse the benchmark CSV file and extract relevant data."""
    try:
        df = pd.read_csv(csv_file)
    except FileNotFoundError:
        print(f"Error: CSV file '{csv_file}' not found!")
        sys.exit(1)
    except Exception as e:
        print(f"Error reading CSV file: {e}")
        sys.exit(1)
    
    # Extract N, M, and solution type from command names
    # Expected format: "N=10_M=25_zero"
    parsed_data = []
    
    for _, row in df.iterrows():
        command_name = row['command']
        mean_time = row['mean']
        
        try:
            # Parse command name: "N=10_M=25_zero"
            parts = command_name.split('_')
            n_part = [p for p in parts if p.startswith('N=')][0]
            m_part = [p for p in parts if p.startswith('M=')][0]
            solution_part = [p for p in parts if p in ['zero', 'one', 'many']][-1]
            
            n_value = int(n_part.split('=')[1])
            m_value = int(m_part.split('=')[1])
            solution_type = solution_part
            
            parsed_data.append({
                'N': n_value,
                'M': m_value,
                'solution_type': solution_type,
                'time': mean_time
            })
            
        except (IndexError, ValueError) as e:
            print(f"Warning: Could not parse command name '{command_name}': {e}")
            continue
    
    if not parsed_data:
        print("Error: No valid data found in CSV file!")
        sys.exit(1)
    
    return pd.DataFrame(parsed_data)

def create_plots(df, output_file='benchmark_plots.png'):
    """Create the two benchmark plots."""
    
    # Filter for zero variant only
    zero_df = df[df['solution_type'] == 'zero'].copy()
    
    if zero_df.empty:
        print("Error: No 'zero' variant data found!")
        sys.exit(1)
    
    # Set up the figure with two subplots
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 5))
    
    # Colors and markers to match the paper plots
    colors = ['#4472C4', '#C5504B', '#70AD47', '#FFC000', '#7030A0']
    markers = ['o', 's', '^', 'D', 'p']
    linestyles = ['-', '--', ':', '-.', '--']
    
    # Get unique N and M values, sorted
    n_values = sorted(zero_df['N'].unique())
    m_values = sorted(zero_df['M'].unique())
    
    # Plot 1: M varying (different N values as series)
    ax1.set_xlabel('M (number of overloads)')
    ax1.set_ylabel('Time (s)')
    ax1.set_title('Runtime vs M for fixed N')
    ax1.grid(True, alpha=0.3)
    
    for i, n in enumerate(n_values):
        n_data = zero_df[zero_df['N'] == n].sort_values('M')
        if not n_data.empty:
            ax1.plot(n_data['M'], n_data['time'], 
                    color=colors[i % len(colors)], 
                    marker=markers[i % len(markers)],
                    linestyle=linestyles[i % len(linestyles)],
                    linewidth=1.2, markersize=6,
                    label=f'N={n}')
    
    ax1.set_xticks(m_values)
    ax1.legend(loc='upper left', fontsize=9)
    
    # Plot 2: N varying (different M values as series)  
    ax2.set_xlabel('N (number of calls)')
    ax2.set_ylabel('Time (s)')
    ax2.set_title('Runtime vs N for fixed M')
    ax2.grid(True, alpha=0.3)
    
    for i, m in enumerate(m_values):
        m_data = zero_df[zero_df['M'] == m].sort_values('N')
        if not m_data.empty:
            ax2.plot(m_data['N'], m_data['time'],
                    color=colors[i % len(colors)],
                    marker=markers[i % len(markers)], 
                    linestyle=linestyles[i % len(linestyles)],
                    linewidth=1.2, markersize=6,
                    label=f'M={m}')
    
    ax2.set_xticks(n_values)
    ax2.legend(loc='upper left', fontsize=9)
    
    # Adjust layout and save
    plt.tight_layout()
    plt.savefig(output_file, dpi=300, bbox_inches='tight')
    print(f"Plots saved to: {output_file}")
    
    return fig

def main():
    parser = argparse.ArgumentParser(description='Plot benchmark results from CSV file')
    parser.add_argument('csv_file', help='Path to the benchmark CSV file')
    parser.add_argument('--output', '-o', default='result_scaling_plots.png',
                       help='Output file for the plots (default: scaling_plots.png)')
    parser.add_argument('--show', action='store_true',
                       help='Display the plots interactively')
    
    args = parser.parse_args()
    
    # Parse the CSV file
    print(f"Reading benchmark data from: {args.csv_file}")
    df = parse_csv(args.csv_file)
    
    # Create and save plots
    fig = create_plots(df, args.output)
    
    if args.show:
        plt.show()
    
    print("Done!")

if __name__ == "__main__":
    main()
