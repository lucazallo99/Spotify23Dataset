# Spotify 2023: Machine Learning Analysis of Most Streamed Songs

## Introduction

This project is my **first machine learning and statistical analysis project** during my master's, focused on analyzing the most streamed Spotify songs of 2023. Using a dataset containing over 900 popular tracks, I explored key factors influencing a song’s success and built predictive models to estimate streaming performance. 

Since this is my first machine learning project, **there is definitely room for improvement**, and I welcome any feedback or suggestions for refinement!

## Dataset

The dataset was sourced from Kaggle:  
[**Most Streamed Spotify Songs 2023**](https://www.kaggle.com/datasets/nelgiriyewithana/top-spotify-songs-2023/data)

It contains **24 variables**, including:
- **Track metadata**: Artist name, track name, release year/month/day, artist count.
- **Streaming statistics**: Number of streams, playlist/chart appearances across different platforms.
- **Musical features**: BPM, danceability, valence, energy, key, mode, and more.

## Objectives

1. **Identify key factors influencing song success**: Explore relationships between song characteristics and streaming numbers.
2. **Develop predictive models**: Train statistical and machine learning models to predict whether a song falls into a high, medium, or low stream category.
3. **Analyze streaming trends**: Examine how factors like **release season, artist collaborations, and musical features** impact popularity.

## Methodology

The analysis was conducted in **R** using statistical tests, regression models, and machine learning techniques:

### **Exploratory Data Analysis (EDA)**
- **Data Cleaning & Feature Engineering**: Removed irrelevant/missing data, categorized variables (e.g., "streams" into Low, Medium, High).
- **Visualization**: Used `ggplot2` to analyze trends and relationships.

### **Statistical Analysis**
- **T-tests & ANOVA**: Examined the influence of release year, artist count, danceability, and energy on streaming numbers.
- **Chi-Square Tests**: Checked categorical relationships, such as release decade vs. musical features.

### **Machine Learning Models**
1. **Linear Regression**: Predicted the number of streams based on numerical features.
2. **Logistic Regression**: Classified whether a song ranked in the **Top 100** based on musical/playlist attributes.
3. **Decision Trees & Random Forest**: Predicted **stream categories** and key influencing factors.

### **Model Performance & Evaluation**
- Used **confusion matrices**, **AIC stepwise selection**, and **cross-validation** to compare model accuracy.

## Key Findings

- **Playlist presence is the biggest predictor of success**: Songs appearing in multiple Spotify and Apple playlists had significantly higher streams.
- **Release timing matters**: Winter releases showed higher average streams.
- **Musical features have limited impact**: Surprisingly, danceability, valence, and BPM had **weak or no significant correlation** with success.
- **Random Forest outperformed other models**: Achieved **~78% accuracy** in classifying songs into stream categories.

## Recommendations

1. **Maximize Playlist Exposure**: Artists should aim to feature on multiple playlists across **Spotify, Apple Music, and Deezer**.
2. **Strategic Release Planning**: Consider releasing new tracks during **Winter** when listener engagement is higher.
3. **Leverage Predictive Models**: Labels can use machine learning to **forecast song success** and optimize marketing strategies.

## Limitations & Future Work

- **Dataset Bias**: Only includes **popular** songs, excluding lesser-known or emerging artists.
- **Evolving Trends**: Streaming preferences change rapidly, making long-term predictions challenging.
- **Model Enhancements**: Future improvements could include **deep learning techniques** and **text-based sentiment analysis** on song lyrics.

## How to Run the Code

1. Clone this repository:
   ```sh
   git clone https://github.com/your-username/your-repo-name.git
   cd your-repo-name
