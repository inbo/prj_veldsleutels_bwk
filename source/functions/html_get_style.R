html_get_style <- function() {
  "
  body {
    font-family: -apple-system, system-ui, sans-serif;
    line-height: 1.6;
    max-width: 800px;
    margin: 20px auto;
    padding: 15px;
    color: #334155;
    background: #f1f5f9;
    font-size: 18px; /* Increased base size */
  }

  h1 { font-size: 1.8rem; color: #1e293b; border-bottom: 3px solid #27ae60; padding-bottom: 10px; text-align: center; }
  h2 { font-size: 1.5rem; }
  h3 { font-size: 1.3rem; }

  /* Index Grid */
  .index-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 20px;
    margin-top: 30px;
  }

  .index-btn {
    display: flex;
    align-items: center;
    justify-content: center;
    height: 100px; /* Larger touch target */
    background: white;
    border: 2px solid #e2e8f0;
    border-radius: 15px;
    font-size: 1.4rem; /* Big text for index */
    text-decoration: none;
    color: #1e293b;
    font-weight: 800;
    box-shadow: 0 4px 6px rgba(0,0,0,0.05);
  }

  /* Containers */
  .step-container {
    background: white;
    padding: 20px;
    margin-bottom: 30px;
    border-radius: 12px;
    box-shadow: 0 4px 10px rgba(0,0,0,0.08);
    border-left: 8px solid #27ae60;
  }

  .step-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 15px; }
  .step-number { background: #27ae60; color: white; padding: 5px 12px; border-radius: 20px; font-weight: bold; font-size: 0.9rem; }
  .incoming-link { background: #e2e8f0; color: #2563eb; text-decoration: none; padding: 4px 10px; border-radius: 6px; font-size: 0.9rem; margin-left: 5px; }

  /* Answers & Navigation */
  .answer-block { margin-bottom: 25px; }

  .answer-link, .answer-link-terminal {
    display: block;
    background: #fff;
    border: 2px solid #cbd5e1;
    padding: 18px 20px; /* Larger padding for fingers */
    text-decoration: none;
    color: #2563eb;
    border-radius: 12px;
    font-weight: 600;
    font-size: 1.1rem; /* Clear answer text */
    transition: 0.2s;
  }

  .answer-link-terminal { color: #1e293b; background: #f0fdf4; border-color: #27ae60; cursor: default; }
  .has-details { border-bottom-left-radius: 0; border-bottom-right-radius: 0; border-bottom: 1px dashed #cbd5e1; }

  .answer-details-group {
    background: #ffffff;
    border: 2px solid #cbd5e1;
    border-top: none;
    border-radius: 0 0 12px 12px;
    padding: 15px;
  }

  .extra-info {
    background: #f0f9ff;
    border-left: 5px solid #0ea5e9;
    padding: 12px;
    margin-bottom: 10px;
    font-size: 1rem;
    color: #0369a1;
  }

  .result-box {
    background: #27ae60;
    color: white;
    padding: 15px;
    border-radius: 10px;
    font-size: 1.05rem;
    margin-top: 10px;
    box-shadow: 0 4px 6px rgba(0,0,0,0.1);
  }
  .result-box strong { color: #dcfce7; font-size: 0.85rem; text-transform: uppercase; }

  .answer-remark { font-style: italic; font-size: 0.95rem; color: #64748b; padding-top: 5px; }
  .other-key { background: #fffbeb; border: 1px solid #fef3c7; color: #92400e; padding: 10px; border-radius: 8px; font-size: 0.95rem; margin-top: 10px; }

  .question { font-size: 1.4rem; font-weight: bold; margin: 20px 0; color: #0f172a; line-height: 1.3; }
  .background-box { background: #f0fdf4; border: 1px solid #dcfce7; padding: 15px; border-radius: 8px; font-size: 1rem; margin-bottom: 15px; }
  .back-to-index { display: inline-block; padding: 12px 24px; background: #e2e8f0; text-decoration: none; border-radius: 10px; color: #475569; font-weight: bold; font-size: 1rem; }
  "
}
