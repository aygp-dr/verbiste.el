#!/usr/bin/env python3
import json
import requests
import numpy as np
import matplotlib.pyplot as plt
from sklearn.metrics.pairwise import cosine_similarity
import click
import os

def get_embeddings(texts, model="nomic-embed-text"):
    """Get embeddings from Ollama API for a list of texts"""
    embeddings = []
    
    for text in texts:
        response = requests.post(
            "http://localhost:11434/api/embeddings",
            json={
                "model": model,
                "prompt": text
            }
        )
        
        if response.status_code == 200:
            result = response.json()
            embeddings.append(result["embedding"])
        else:
            click.echo(f"Error for text '{text}': {response.text}", err=True)
            
    return np.array(embeddings)

def analyze_verb_similarities(verbs, model, output_file):
    """Analyze similarities between French verbs using embeddings"""
    click.echo(f"Getting embeddings for {len(verbs)} verbs using model {model}...")
    embeddings = get_embeddings(verbs, model)
    
    click.echo("Calculating similarity matrix...")
    sim_matrix = cosine_similarity(embeddings)
    
    click.echo(f"Plotting and saving to {output_file}...")
    plt.figure(figsize=(10, 8))
    plt.imshow(sim_matrix, cmap='viridis')
    plt.colorbar()
    plt.xticks(np.arange(len(verbs)), verbs, rotation=45)
    plt.yticks(np.arange(len(verbs)), verbs)
    plt.title('Semantic Similarity Between French Verbs')
    plt.tight_layout()
    plt.savefig(output_file)
    
    return sim_matrix

@click.command()
@click.option('--input-file', '-i', type=click.Path(exists=True), 
              help='JSON file containing a list of French verbs')
@click.option('--verbs', '-v', multiple=True, 
              help='Specify verbs directly (can be used multiple times)')
@click.option('--model', '-m', default='nomic-embed-text', 
              help='Embedding model to use from Ollama')
@click.option('--output', '-o', default='french_verb_similarity.png', 
              help='Output file for similarity matrix visualization')
@click.option('--save-matrix', '-s', is_flag=True, 
              help='Save the similarity matrix to a numpy file')
def main(input_file, verbs, model, output, save_matrix):
    """Analyze semantic similarities between French verbs using embeddings from Ollama."""
    
    if input_file and os.path.exists(input_file):
        with open(input_file, 'r') as f:
            french_verbs = json.load(f)
        click.echo(f"Loaded {len(french_verbs)} verbs from {input_file}")
    elif verbs:
        french_verbs = list(verbs)
        click.echo(f"Using {len(french_verbs)} verbs provided via command line")
    else:
        # Example French verbs as default
        french_verbs = [
            "aller", "venir", "faire", "dire", "voir", 
            "savoir", "pouvoir", "vouloir", "devoir", "prendre",
            "parler", "manger", "boire", "dormir", "courir"
        ]
        click.echo(f"Using {len(french_verbs)} default example verbs")
    
    # Analyze and visualize similarities
    sim_matrix = analyze_verb_similarities(french_verbs, model, output)
    click.echo(f"Similarity matrix computed and saved to {output}")
    
    if save_matrix:
        matrix_file = os.path.splitext(output)[0] + '.npy'
        np.save(matrix_file, sim_matrix)
        click.echo(f"Similarity matrix saved to {matrix_file}")

if __name__ == "__main__":
    main()
