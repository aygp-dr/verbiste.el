#!/usr/bin/env python3
import numpy as np
import matplotlib.pyplot as plt
import json
from sklearn.cluster import KMeans, AgglomerativeClustering
from sklearn.manifold import TSNE
import click
import os

def cluster_verbs(embeddings, verbs, n_clusters=3, method='kmeans'):
    """Cluster verbs based on their embeddings"""
    if method == 'kmeans':
        clustering = KMeans(n_clusters=n_clusters, random_state=42)
    elif method == 'hierarchical':
        clustering = AgglomerativeClustering(n_clusters=n_clusters)
    else:
        raise ValueError(f"Unknown clustering method: {method}")
    
    # Fit clustering
    cluster_labels = clustering.fit_predict(embeddings)
    
    # Group verbs by cluster
    clusters = {}
    for i, label in enumerate(cluster_labels):
        if label not in clusters:
            clusters[label] = []
        clusters[label].append(verbs[i])
    
    return clusters, cluster_labels

def visualize_clusters(embeddings, verbs, labels, output_file):
    """Create a t-SNE visualization of clustered verbs"""
    # Reduce dimensionality for visualization
    tsne = TSNE(n_components=2, random_state=42, perplexity=min(5, len(embeddings)-1))
    embeddings_2d = tsne.fit_transform(embeddings)
    
    # Plot
    plt.figure(figsize=(12, 10))
    
    # Get unique labels and assign colors
    unique_labels = set(labels)
    colors = plt.cm.rainbow(np.linspace(0, 1, len(unique_labels)))
    
    # Plot each cluster
    for label, color in zip(unique_labels, colors):
        mask = labels == label
        plt.scatter(
            embeddings_2d[mask, 0], 
            embeddings_2d[mask, 1], 
            c=[color], 
            label=f'Cluster {label}',
            s=100
        )
    
    # Add verb labels
    for i, verb in enumerate(verbs):
        plt.annotate(
            verb, 
            (embeddings_2d[i, 0], embeddings_2d[i, 1]),
            fontsize=12,
            ha='center',
            va='center',
            bbox=dict(boxstyle='round,pad=0.3', fc='white', alpha=0.7)
        )
    
    plt.title('t-SNE Visualization of French Verb Clusters')
    plt.legend()
    plt.grid(True, linestyle='--', alpha=0.7)
    plt.tight_layout()
    plt.savefig(output_file)
    plt.close()

@click.command()
@click.option('--embeddings-file', '-e', type=click.Path(exists=True), required=True,
              help='NumPy file containing verb embeddings')
@click.option('--verbs-file', '-v', type=click.Path(exists=True), required=True,
              help='JSON file containing verb list')
@click.option('--n-clusters', '-n', default=3, type=int,
              help='Number of clusters to create')
@click.option('--method', '-m', default='kmeans', type=click.Choice(['kmeans', 'hierarchical']),
              help='Clustering method to use')
@click.option('--output', '-o', default='verb_clusters.png',
              help='Output file for cluster visualization')
def main(embeddings_file, verbs_file, n_clusters, method, output):
    """Cluster French verbs based on embedding similarity"""
    # Load data
    embeddings = np.load(embeddings_file)
    with open(verbs_file, 'r') as f:
        verbs = json.load(f)
    
    # Ensure data dimensions match
    if len(verbs) != len(embeddings):
        click.echo(f"Error: Number of verbs ({len(verbs)}) does not match number of embeddings ({len(embeddings)})")
        return
    
    # Perform clustering
    click.echo(f"Clustering {len(verbs)} verbs into {n_clusters} clusters using {method}...")
    clusters, labels = cluster_verbs(embeddings, verbs, n_clusters, method)
    
    # Print clusters
    for label, cluster_verbs in clusters.items():
        click.echo(f"Cluster {label}: {', '.join(cluster_verbs)}")
    
    # Visualize clusters
    click.echo(f"Creating cluster visualization and saving to {output}...")
    visualize_clusters(embeddings, verbs, labels, output)
    
    # Save clusters to file
    base_filename = os.path.splitext(output)[0]
    clusters_file = f"{base_filename}.json"
    with open(clusters_file, 'w') as f:
        json.dump(clusters, f, indent=2)
    click.echo(f"Cluster assignments saved to {clusters_file}")

if __name__ == "__main__":
    main()
