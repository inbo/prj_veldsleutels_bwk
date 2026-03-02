# get_css_style.R

get_css_style <- function() {
  styletext <- "
  /* Base styles */
  * {
    box-sizing: border-box;
  }

  body {
    background-color: #e6f2f7;
    font-family: 'Arial', sans-serif;
    font-size: 18px;
    line-height: 1.6;
    color: #333;
    max-width: 1200px;
    margin: 0 auto;
    padding: 15px;
  }

  h1 {
    color: #1a5276;
    padding-bottom: 10px;
    border-bottom: 2px solid #1a5276;
    margin-bottom: 25px;
  }

  h2, h3, h4 {
    color: #2874a6;
    margin-top: 25px;
    margin-bottom: 15px;
  }

  a {
    color: #2874a6;
    text-decoration: none;
    transition: color 0.3s;
  }

  a:hover {
    color: #1a5276;
    text-decoration: underline;
  }

  /* Table styles */
  table {
    width: 100%;
    margin: 20px 0;
    border-collapse: collapse;
    box-shadow: 0 2px 5px rgba(0,0,0,0.1);
  }

  th, td {
    border: 1px solid #b3d9e6;
    padding: 12px;
    text-align: left;
  }

  th {
    background-color: #b3d9e6;
    color: #1a5276;
  }

  tr:nth-child(even) {
    background-color: #f2f9fc;
  }

  tr:hover {
    background-color: #d9ebf2;
  }

  /* Step styling */
  .step-container {
    background-color: white;
    border-radius: 8px;
    margin-bottom: 30px;
    padding: 20px;
    box-shadow: 0 2px 5px rgba(0,0,0,0.1);
  }

  .step-header {
    background-color: #d9ebf2;
    padding: 15px;
    margin: -20px -20px 20px -20px;
    border-radius: 8px 8px 0 0;
    position: relative;
  }

  .step-number {
    font-weight: bold;
    color: #1a5276;
  }

  .step-incoming {
    font-size: 0.9em;
    color: #666;
    margin-top: 5px;
  }

  .step-remark {
    background-color: #fff9e0;
    padding: 10px;
    border-left: 4px solid #ffd700;
    margin: 15px 0;
    font-style: italic;
  }

  /* Answer styling */
  .answer {
    background-color: #f7f9fa;
    border-left: 4px solid #2874a6;
    padding: 15px;
    margin: 15px 0;
    border-radius: 0 8px 8px 0;
    transition: background-color 0.3s;
  }

  .answer:hover {
    background-color: #e6f2f7;
  }

  .answer a {
    display: block;
    font-weight: bold;
  }

  .answer-remark {
    font-style: italic;
    color: #666;
    margin-top: 10px;
  }

  .extra-info {
    background-color: #f0f7fa;
    border: 1px solid #d1e8f0;
    border-radius: 8px;
    padding: 15px;
    margin: 15px 0;
  }

  .result {
    background-color: #e8f5e9;
    color: #2e7d32;
    padding: 10px 15px;
    border-radius: 8px;
    margin-top: 15px;
    font-weight: bold;
  }

  .other-key {
    background-color: #e8eaf6;
    padding: 10px 15px;
    border-radius: 8px;
    margin-top: 15px;
  }

  /* Back to index button */
  .back-to-index {
    position: fixed;
    bottom: 20px;
    right: 20px;
    background-color: #2874a6;
    color: white;
    padding: 10px 15px;
    border-radius: 30px;
    box-shadow: 0 2px 10px rgba(0,0,0,0.2);
    z-index: 100;
  }

  .back-to-index a {
    color: white;
    text-decoration: none;
    font-weight: bold;
  }

  /* Responsive design */
  @media (max-width: 768px) {
    body {
      font-size: 16px;
      padding: 10px;
    }

    .step-container {
      padding: 15px;
    }

    .step-header {
      padding: 12px;
      margin: -15px -15px 15px -15px;
    }

    .answer, .step-remark, .extra-info {
      padding: 12px;
    }
  }
  "
  return(styletext)
}
