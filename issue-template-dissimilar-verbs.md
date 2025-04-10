# Feature: Add dissimilar verb analysis and topic-based clustering

## Description
Currently, our verb cluster functionality only shows verbs that are similar to each other based on embedding similarity scores. We need additional functionality to:

1. Identify the most dissimilar verbs (furthest in embedding space) for better understanding of semantic relationships
2. Implement topic-based clustering (using KMeans or similar algorithms) to group verbs by thematic areas such as:
   - Cooking/Food
   - Shopping/Commerce
   - Travel/Movement
   - Education/Study
   - Communication
   - Daily routine

These groupings would be particularly useful for language learners following typical textbook organization.

## Implementation Details

### Dissimilar Verbs Analysis
The current approach only finds similar verbs by taking the highest cosine similarity scores. To find dissimilar verbs:

1. Load the complete embeddings from `french_verb_embeddings.json` (not just the clusters file)
2. For each verb, calculate the cosine distance (1 - similarity) to all other verbs
3. Sort by distance (descending) to find the most semantically distant verbs
4. Add a new function `verbiste--get-dissimilar-verbs` to the Emacs interface

### Topic-Based Clustering
1. Apply KMeans clustering on the verb embeddings with k=8-12 clusters
2. Manually review and label the resulting clusters with appropriate thematic names
3. Create a new data structure to store the topic-based clusters
4. Add functions to browse verbs by topic area

## Benefits
- Language learners can explore verbs by thematic areas matching textbook organization
- More comprehensive understanding of semantic relationships between verbs
- Enhanced exploration-based learning experience
- Will improve the graph-like exploration feature by providing multiple navigation paths

## Technical Requirements
- Add Python script: `generate_verb_topics.py` 
- Add Emacs Lisp functions: `verbiste-browse-topics` and `verbiste--get-dissimilar-verbs`
- Update UI to include topic-based navigation
- Create topic labels and descriptions for clusters