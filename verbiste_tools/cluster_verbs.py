#!/usr/bin/env python3
"""
Verb clustering tool for Verbiste.
This script analyzes verb conjugation patterns and clusters similar verbs.
"""

import argparse
import json
import os
import sys
from typing import Dict, List, Tuple, Set

import numpy as np
import pandas as pd
from sklearn.cluster import KMeans, AgglomerativeClustering
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.metrics import silhouette_score
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt


def load_data(verbs_file: str, conjugation_file: str) -> Tuple[Dict, Dict]:
    """Load verb data from JSON files."""
    with open(verbs_file, 'r', encoding='utf-8') as f:
        verbs_data = json.load(f)
    
    with open(conjugation_file, 'r', encoding='utf-8') as f:
        conjugation_data = json.load(f)
    
    return verbs_data, conjugation_data


def extract_patterns(verb: str, template: str, conjugation_data: Dict) -> List[str]:
    """Extract conjugation patterns for a verb."""
    patterns = []
    
    try:
        template_data = conjugation_data['templates'].get(template)
        if not template_data:
            return patterns
        
        # Extract stem from infinitive
        infinitive_pattern = template_data.get('infinitive', '')
        if infinitive_pattern.endswith('er'):
            stem = verb[:-2]  # For -er verbs
        elif infinitive_pattern.endswith('ir'):
            stem = verb[:-2]  # For -ir verbs
        elif infinitive_pattern.endswith('re'):
            stem = verb[:-2]  # For -re verbs
        else:
            stem = verb
        
        # Collect conjugation patterns
        for tense_name, tense_data in template_data.get('tenses', {}).items():
            for person, pattern in tense_data.items():
                if pattern:
                    # Replace stem placeholder with actual stem
                    actual_form = pattern.replace('#', stem)
                    patterns.append(f"{verb}:{tense_name}:{person}:{actual_form}")
        
        return patterns
    except Exception as e:
        print(f"Error extracting patterns for {verb}: {e}", file=sys.stderr)
        return patterns


def create_feature_matrix(verbs_data: Dict, conjugation_data: Dict) -> pd.DataFrame:
    """Create feature matrix for clustering."""
    verb_features = {}
    
    for verb, info in verbs_data.get('verbs', {}).items():
        template = info.get('template')
        if template:
            patterns = extract_patterns(verb, template, conjugation_data)
            if patterns:
                # Join all patterns into a single string for text vectorization
                verb_features[verb] = ' '.join(patterns)
    
    # Convert to DataFrame
    df = pd.DataFrame.from_dict(verb_features, orient='index', columns=['patterns'])
    
    return df


def cluster_verbs(df: pd.DataFrame, n_clusters: int = 10, method: str = 'kmeans') -> pd.DataFrame:
    """Cluster verbs based on conjugation patterns."""
    # Create a text vectorizer for the patterns
    vectorizer = CountVectorizer(analyzer='char', ngram_range=(2, 4))
    
    # Apply vectorization
    X = vectorizer.fit_transform(df['patterns'])
    
    # Apply clustering
    if method == 'kmeans':
        clusterer = KMeans(n_clusters=n_clusters, random_state=42)
    else:  # Hierarchical clustering
        clusterer = AgglomerativeClustering(n_clusters=n_clusters)
    
    # Get cluster labels
    if method == 'kmeans':
        df['cluster'] = clusterer.fit_predict(X)
    else:
        df['cluster'] = clusterer.fit_predict(X.toarray())
    
    # Get silhouette score
    if X.shape[0] > n_clusters + 1:  # Need more samples than clusters for silhouette
        silhouette = silhouette_score(X, df['cluster'])
        print(f"Silhouette score: {silhouette:.4f}")
    
    return df


def output_clusters(df: pd.DataFrame, output_dir: str) -> None:
    """Save clustering results to files."""
    os.makedirs(output_dir, exist_ok=True)
    
    # Save full results
    df.to_csv(os.path.join(output_dir, 'verb_clusters.csv'))
    
    # Save individual cluster files
    for cluster_id in df['cluster'].unique():
        cluster_verbs = df[df['cluster'] == cluster_id].index.tolist()
        with open(os.path.join(output_dir, f'cluster_{cluster_id}.txt'), 'w', encoding='utf-8') as f:
            for verb in sorted(cluster_verbs):
                f.write(f"{verb}\n")
    
    # Create a visualization
    plt.figure(figsize=(12, 8))
    cluster_sizes = df['cluster'].value_counts().sort_index()
    cluster_sizes.plot(kind='bar')
    plt.title('Verb Clusters by Size')
    plt.xlabel('Cluster ID')
    plt.ylabel('Number of Verbs')
    plt.tight_layout()
    plt.savefig(os.path.join(output_dir, 'cluster_sizes.png'))
    print(f"Saved results to {output_dir}")


def main() -> int:
    """Main entry point."""
    parser = argparse.ArgumentParser(description='Cluster French verbs based on conjugation patterns')
    parser.add_argument('--verbs', required=True, help='Path to verbs-fr.json')
    parser.add_argument('--conjugation', required=True, help='Path to conjugation-fr.json')
    parser.add_argument('--output', required=True, help='Output directory for results')
    parser.add_argument('--clusters', type=int, default=10, help='Number of clusters')
    parser.add_argument('--method', choices=['kmeans', 'hierarchical'], default='kmeans', help='Clustering method')
    
    args = parser.parse_args()
    
    try:
        # Load data
        verbs_data, conjugation_data = load_data(args.verbs, args.conjugation)
        
        # Create feature matrix
        df = create_feature_matrix(verbs_data, conjugation_data)
        if df.empty:
            print("No verb patterns extracted.", file=sys.stderr)
            return 1
        
        # Cluster verbs
        clustered_df = cluster_verbs(df, args.clusters, args.method)
        
        # Output results
        output_clusters(clustered_df, args.output)
        
        return 0
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())