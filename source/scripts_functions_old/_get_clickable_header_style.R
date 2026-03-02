get_clickable_header_style <- function() {
  clickable_header_style <- "
.clickable-header {
    background-color: #ffffff;
    border-radius: 8px;
    padding: 20px;
    margin: 15px 0;
    cursor: pointer;
    display: block;
    color: #2874a6;
    text-decoration: none;
    font-size: 1.2em;
    font-weight: bold;
    box-shadow: 0 2px 8px rgba(0,0,0,0.1);
    transition: all 0.3s ease;
    border-left: 5px solid #2874a6;
}

.clickable-header:hover {
    background-color: #f2f9fc;
    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
    transform: translateY(-2px);
}

.clickable-header:active {
    background-color: #e6f2f7;
    transform: translateY(1px);
    box-shadow: 0 1px 4px rgba(0,0,0,0.1);
}

@media (max-width: 768px) {
    .clickable-header {
        padding: 15px;
        margin: 12px 0;
    }
}
"
  return(clickable_header_style)
}
